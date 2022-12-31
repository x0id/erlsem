#include <erl_nif.h>

#include <atomic>
#include <mutex>
#include <map>

#ifdef TRACE
#include <iostream>
#endif

static ErlNifResourceType* SEMA;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_backlog_full;
static ERL_NIF_TERM atom_duplicate_pid;
static ERL_NIF_TERM atom_not_found;

inline ERL_NIF_TERM mk_atom(ErlNifEnv *env, const char *name) {
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;
    return enif_make_atom(env, name);
}

inline ERL_NIF_TERM error_tuple(ErlNifEnv *env, ERL_NIF_TERM what) {
    return enif_make_tuple2(env, atom_error, what);
}

inline ERL_NIF_TERM ok_tuple(ErlNifEnv *env, ERL_NIF_TERM ret) {
    return enif_make_tuple2(env, atom_ok, ret);
}

inline ERL_NIF_TERM unsigned_result(ErlNifEnv *env, unsigned ret) {
    return ok_tuple(env, enif_make_uint(env, ret));
}

// this overload is needed by std::map<ErlNifPid>
inline bool operator<(const ErlNifPid& lhs, const ErlNifPid& rhs) {
    return enif_compare_pids(&lhs, &rhs) < 0;
}

template<typename T>
struct enif_allocator : std::allocator<T> {
    using typename std::allocator<T>::pointer;
    using typename std::allocator<T>::size_type;

    template<typename U>
    struct rebind { typedef enif_allocator<U> other; };

    enif_allocator() noexcept : std::allocator<T>() {}

    template<typename U>
    enif_allocator(enif_allocator<U> const& u) noexcept
        : std::allocator<T>(u)
    {}

    pointer allocate(size_type size, const void *hint = 0) {
        void *p = enif_alloc(size * sizeof(T));
        if (p == 0) throw std::bad_alloc();
        return static_cast<pointer>(p);
    }

    void deallocate(pointer p, size_type size) {
        enif_free(p);
    }
};

struct sema {
    std::atomic_uint cnt;
    unsigned max;

    std::map<
        ErlNifPid,
        ErlNifMonitor,
        std::less<ErlNifPid>,
        enif_allocator<std::pair<const ErlNifPid, ErlNifMonitor>>
    > pids;
    std::mutex pid_mutex;

    static ErlNifMonitor null_mon;

    sema(unsigned n) : cnt(0), max(n) {}

    ERL_NIF_TERM try_to_take(ErlNifEnv *env, const ErlNifPid& pid, bool mon) {
        unsigned x;
        while ((x = cnt.load(std::memory_order_acquire)) < max) {
            if (cnt.compare_exchange_strong(
                x,
                x + 1,
                std::memory_order_release,
                std::memory_order_relaxed
            )) {
                ErlNifMonitor proc_mon = null_mon;
                if (!mon || monitor_pid(env, pid, proc_mon)) {
                    if (add_pid(pid, proc_mon)) {
                        // process added to the registry, return new count
                        return unsigned_result(env, x + 1);
                    } else {
                        // process already registered, roll back
                        cnt.fetch_sub(1, std::memory_order_acq_rel);
                        return error_tuple(env, atom_duplicate_pid);
                    }
                } else {
                    // process we're trying to monitor already died
                    return error_tuple(env, atom_not_found);
                }
            } else {
                // CAS failed - try again
                continue;
            }
        }
        // max reached
        return error_tuple(env, atom_backlog_full);
    }

    bool monitor_pid(ErlNifEnv *env, const ErlNifPid& pid, ErlNifMonitor& mon) {
        return 0 == enif_monitor_process(env, this, &pid, &mon);
    }

    bool add_pid(const ErlNifPid& pid, const ErlNifMonitor& mon) {
        // activate mutex guard
        const std::lock_guard<std::mutex> lock(pid_mutex);

        // register pid
        return pids.emplace(pid, mon).second;
    }

    // unregister presumably "live" process, optionally demonitor it
    bool del_pid(ErlNifEnv *env, const ErlNifPid& pid) {
        // activate mutex guard
        const std::lock_guard<std::mutex> lock(pid_mutex);

        // check if the pid exists in our table
        auto it = pids.find(pid);
        if (it == pids.end())
            return false;

        // optionally demonitor the process
        auto proc_mon = it->second;
        if (enif_compare_monitors(&proc_mon, &null_mon) != 0)
            enif_demonitor_process(env, this, &proc_mon);

        // deregister pid by removing from the pids table
        pids.erase(it);
        return true;
    }

    // unregister (garbage-collect) "dead" procees
    void gc_pid(const ErlNifPid& pid) {
        const std::lock_guard<std::mutex> lock(pid_mutex);
        pids.erase(pid);
    }

    ERL_NIF_TERM vacate(ErlNifEnv *env, const ErlNifPid& pid) {
        if (del_pid(env, pid)) {
            // process removed from registry, decrement and return new count
            return unsigned_result(env,
                cnt.fetch_sub(1, std::memory_order_acq_rel) - 1);
        } else {
            // process not found in the registry
            return error_tuple(env, atom_not_found);
        }
    }
};

ErlNifMonitor sema::null_mon = {0};

static void free_sema(ErlNifEnv *env, void *obj) {
    if (obj != nullptr) {
        sema& x = *(sema *)obj;
#ifdef TRACE
        auto n = x.cnt.load(std::memory_order_acquire);
        std::cout << "free> cnt: " << n << ", max: " << x.max << "\r\n";
#endif
        x.~sema();
    }
}

static void
proc_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon) {
    if (obj != nullptr && pid != nullptr) {
        ((sema *)obj)->gc_pid(*pid);
#ifdef TRACE
        enif_fprintf(stdout, "process %T down\r\n", *pid);
#endif
    }
}

static bool open_resource(ErlNifEnv *env) {
    auto flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    ErlNifResourceTypeInit init = {.dtor = free_sema, .down = proc_down};
    SEMA = enif_open_resource_type_x(env, "sema", &init, flags, nullptr);
    return SEMA != nullptr;
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    if (!open_resource(env)) return -1;
    atom_ok = mk_atom(env, "ok");
    atom_error = mk_atom(env, "error");
    atom_true = mk_atom(env, "true");
    atom_backlog_full = mk_atom(env, "backlog_full");
    atom_duplicate_pid = mk_atom(env, "duplicate_pid");
    atom_not_found = mk_atom(env, "not_found");
    return 0;
}

static ERL_NIF_TERM
create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1)
        return enif_make_badarg(env);

    unsigned max;
    if (!enif_get_uint(env, argv[0], &max))
        return enif_make_badarg(env);

    void *res = enif_alloc_resource(SEMA, sizeof(sema));
    if (res == nullptr)
        return enif_make_badarg(env);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);

    new (res) sema(max);

    return ret;
}

static ERL_NIF_TERM
occupy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc < 2 || argc > 3)
        return enif_make_badarg(env);

    ErlNifPid pid;
    if (!enif_is_pid(env, argv[1]))
        return enif_make_badarg(env);

    if (!enif_get_local_pid(env, argv[1], &pid))
        return enif_make_badarg(env);

    sema *res = nullptr;
    if (!enif_get_resource(env, argv[0], SEMA, (void **)&res))
        return enif_make_badarg(env);

    if (res == nullptr)
        return enif_make_badarg(env);

    bool monitor = false;
    if (argc == 3) {
        if (!enif_is_atom(env, argv[2]))
            return enif_make_badarg(env);
        monitor = enif_compare(atom_true, argv[2]);
    }

    return res->try_to_take(env, pid, monitor);
}

static ERL_NIF_TERM
vacate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2)
        return enif_make_badarg(env);

    ErlNifPid pid;
    if (!enif_is_pid(env, argv[1]))
        return enif_make_badarg(env);

    if (!enif_get_local_pid(env, argv[1], &pid))
        return enif_make_badarg(env);

    sema *res = nullptr;
    if (!enif_get_resource(env, argv[0], SEMA, (void **)&res))
        return enif_make_badarg(env);

    if (res == nullptr)
        return enif_make_badarg(env);

    return res->vacate(env, pid);
}

static ErlNifFunc nif_funcs[] = {
    {"create", 1, create},
    {"occupy", 2, occupy},
    {"occupy", 3, occupy},
    {"vacate", 2, vacate}
};

ERL_NIF_INIT(sema_nif, nif_funcs, &load, nullptr, nullptr, nullptr);
