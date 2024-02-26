#include <erl_nif.h>

#include <atomic>
#include <mutex>
#include <map>
#include <cassert>

#ifdef TRACE
#include <iostream>
#endif

static ErlNifResourceType* SEMA;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_full;
static ERL_NIF_TERM atom_not_found;
static ERL_NIF_TERM atom_dead;
static ERL_NIF_TERM atom_cnt;
static ERL_NIF_TERM atom_max;

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
    std::atomic_uint dead_counter;
    unsigned max;

    typedef std::pair<ErlNifMonitor, unsigned> mon_cnt_t;
    std::map<
        ErlNifPid,
        mon_cnt_t,
        std::less<ErlNifPid>,
        enif_allocator<std::pair<const ErlNifPid, mon_cnt_t>>
    > pids;
    std::mutex pid_mutex;

    static ErlNifMonitor null_mon;

    sema(unsigned n) : cnt(0), dead_counter(0), max(n) {}

    ERL_NIF_TERM info(ErlNifEnv *env) {
        ERL_NIF_TERM keys[] = {atom_dead, atom_cnt, atom_max};
        ERL_NIF_TERM vals[] = {
            enif_make_uint(env, dead_counter.load(std::memory_order_acquire)),
            enif_make_uint(env, cnt.load(std::memory_order_acquire)),
            enif_make_uint(env, max)
        };
        unsigned n = sizeof(vals) / sizeof(vals[0]);
        ERL_NIF_TERM map_ret;
        enif_make_map_from_arrays(env, keys, vals, n, &map_ret);
        return map_ret;
    }

    unsigned capacity() const     { return max; }
    unsigned capacity(unsigned n) { auto old=max; max=n; return old; }

    ERL_NIF_TERM try_to_take(ErlNifEnv *env, const ErlNifPid& pid, unsigned n) {
        unsigned x = cnt.load(std::memory_order_relaxed);
        while (x + n <= max) {
            if (cnt.compare_exchange_weak(
                x,
                x + n,
                std::memory_order_release,
                std::memory_order_relaxed
            )) {
                if (maybe_monitor(env, pid, n)) {
                    // process added to the registry, return new count
                    return unsigned_result(env, x + n);
                } else {
                    // process we're trying to monitor is already died
                    return error_tuple(env, atom_not_found);
                }
            } else {
                // CAS failed - try again
                continue;
            }
        }
        // max reached
        return error_tuple(env, atom_full);
    }

    bool maybe_monitor(ErlNifEnv *env, const ErlNifPid& pid, unsigned n) {
        // activate mutex guard
        const std::lock_guard<std::mutex> lock(pid_mutex);

        // check if the pid exists in our table
        auto it = pids.find(pid);
        if (it == pids.end()) {
            // process not yet registered, monitor it
            ErlNifMonitor mon = null_mon;
            if (monitor_pid(env, pid, mon)) {
                pids.emplace(
                    std::piecewise_construct,
                    std::forward_as_tuple(pid),
                    std::forward_as_tuple(mon, n)
                );
                return true;
            } else {
                // process already gone
                return false;
            }
        } else {
            it->second.second += n;
            return true;
        }
    }

    bool monitor_pid(ErlNifEnv *env, const ErlNifPid& pid, ErlNifMonitor& mon) {
        return 0 == enif_monitor_process(env, this, &pid, &mon);
    }

    // unregister presumably "live" process, optionally demonitor it
    bool del_pid(ErlNifEnv *env, const ErlNifPid& pid, unsigned n) {
        // activate mutex guard
        const std::lock_guard<std::mutex> lock(pid_mutex);

        // check if the pid exists in our table
        auto it = pids.find(pid);
        if (it == pids.end())
            return false;

        // optionally demonitor the process
        if ((it->second.second -= n) < 1) {
            auto proc_mon = it->second.first;
            if (enif_compare_monitors(&proc_mon, &null_mon) != 0)
                enif_demonitor_process(env, this, &proc_mon);

            // deregister pid by removing from the pids table
            pids.erase(it);
        }
        return true;
    }

    // unregister (garbage-collect) "dead" process
    void gc_pid(const ErlNifPid& pid) {
        const std::lock_guard<std::mutex> lock(pid_mutex);

        // look for the process data
        auto it = pids.find(pid);
        if (it != pids.end()) {
            unsigned n = it->second.second;
            // release resource units occupied by the pid
            cnt.fetch_sub(n, std::memory_order_acquire);
        }

        // increment dead counter
        dead_counter.fetch_add(1, std::memory_order_release);

        // deregister pid by removing from the pids table
        pids.erase(pid);
    }

    ERL_NIF_TERM release(ErlNifEnv *env, const ErlNifPid& pid, unsigned n) {
        if (del_pid(env, pid, n)) {
            // process found, decrement and return new count
            return unsigned_result(env,
                cnt.fetch_sub(n, std::memory_order_acq_rel) - n);
        } else {
            // process not found in the registry
            return error_tuple(env, atom_not_found);
        }
    }
};

ErlNifMonitor sema::null_mon;

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
    atom_full = mk_atom(env, "full");
    atom_not_found = mk_atom(env, "not_found");
    atom_dead = mk_atom(env, "dead");
    atom_cnt = mk_atom(env, "cnt");
    atom_max = mk_atom(env, "max");
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
info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);

    sema *res = nullptr;
    if (!enif_get_resource(env, argv[0], SEMA, (void **)&res))
        return enif_make_badarg(env);

    if (res == nullptr)
        return enif_make_badarg(env);

    return res->info(env);
}

static ERL_NIF_TERM
capacity(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc >= 1 && argc <= 2);

    sema *res = nullptr;
    if (!enif_get_resource(env, argv[0], SEMA, (void **)&res) || !res)
        return enif_make_badarg(env);

    unsigned max = 0;
    if (argc == 2 && !enif_get_uint(env, argv[1], &max))
        return enif_make_badarg(env);
        
    return enif_make_uint(env, argc == 2 ? res->capacity(max) : res->capacity());
}

static ERL_NIF_TERM
acquire(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc < 1 || argc > 2)
        return enif_make_badarg(env);

    sema *res = nullptr;
    if (!enif_get_resource(env, argv[0], SEMA, (void **)&res))
        return enif_make_badarg(env);

    if (res == nullptr)
        return enif_make_badarg(env);

    unsigned n = 1;
    if (argc > 1) {
        if (enif_is_number(env, argv[1]))
            enif_get_uint(env, argv[1], &n);
        else
            return enif_make_badarg(env);
        if (n < 1)
            return enif_make_badarg(env);
    }

    ErlNifPid pid = {};

    if (nullptr == enif_self(env, &pid))
        return enif_make_badarg(env);

    return res->try_to_take(env, pid, n);
}

static ERL_NIF_TERM
release(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc < 1 || argc > 3)
        return enif_make_badarg(env);

    sema *res = nullptr;
    if (!enif_get_resource(env, argv[0], SEMA, (void **)&res))
        return enif_make_badarg(env);

    if (res == nullptr)
        return enif_make_badarg(env);

    ErlNifPid pid = {};
    unsigned n = 1;

    int k = 1;
    if (k < argc) {
        const ERL_NIF_TERM& term = argv[k];
        if (enif_is_number(env, term)) {
            enif_get_uint(env, term, &n);
            if (n < 1)
                return enif_make_badarg(env);
            ++k;
        }
    }
    if (k < argc) {
        const ERL_NIF_TERM& term = argv[k];
        if (enif_is_pid(env, term)) {
            if (!enif_get_local_pid(env, term, &pid))
                return enif_make_badarg(env);
        }
        else
            return enif_make_badarg(env);
    } else {
        if (nullptr == enif_self(env, &pid))
            return enif_make_badarg(env);
    }

    return res->release(env, pid, n);
}

static ErlNifFunc nif_funcs[] = {
    {"create",   1, create},
    {"info",     1, info},
    {"capacity", 1, capacity},
    {"capacity", 2, capacity},
    {"acquire",  1, acquire},
    {"acquire",  2, acquire},
    {"release",  1, release},
    {"release",  2, release},
    {"release",  3, release}
};

ERL_NIF_INIT(sema_nif, nif_funcs, &load, nullptr, nullptr, nullptr);
