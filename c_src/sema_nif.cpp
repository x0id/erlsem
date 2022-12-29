#include <erl_nif.h>

#include <atomic>
#include <mutex>
#include <set>

#ifdef TRACE
#include <iostream>
#endif

static ErlNifResourceType* SEMA;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
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

// this overload is needed by std::set<ErlNifPid>
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

    std::set<
        ErlNifPid,
        std::less<ErlNifPid>,
        enif_allocator<ErlNifPid>
    > pids;
    std::mutex pid_mutex;

    sema(unsigned n) : cnt(0), max(n) {}

    ERL_NIF_TERM try_to_take(ErlNifEnv *env, ErlNifPid pid) {
        unsigned x;
        while ((x = cnt.load(std::memory_order_acquire)) < max) {
            if (cnt.compare_exchange_strong(
                x,
                x + 1,
                std::memory_order_release,
                std::memory_order_relaxed
            )) {
                // register pid
                const std::lock_guard<std::mutex> lock(pid_mutex);
                auto ret = pids.insert(pid);
                if (ret.second) {
                    // process added to the registry, return new count
                    return unsigned_result(env, x + 1);
                } else {
                    // process already registered, roll back
                    cnt.fetch_sub(1, std::memory_order_release);
                    return error_tuple(env, atom_duplicate_pid);
                }
            } else {
                // CAS failed - try again
                continue;
            }
        }
        // max reached
        return error_tuple(env, atom_backlog_full);
    }

    ERL_NIF_TERM vacate(ErlNifEnv *env, ErlNifPid pid) {
        const std::lock_guard<std::mutex> lock(pid_mutex);
        if (pids.erase(pid) > 0) {
            // process removed from registry, decrement and return new count
            unsigned n = cnt.fetch_sub(1, std::memory_order_release);
            return unsigned_result(env, n - 1);
        } else {
            // process not found in the registry
            return error_tuple(env, atom_not_found);
        }
    }
};

static void free_sema(ErlNifEnv *env, void *obj) {
    if (obj != nullptr) {
        sema& x = *(sema*)obj;
#ifdef TRACE
        auto n = x.cnt.load();
        std::cout << "free> cnt: " << n << ", max: " << x.max << "\r\n";
#endif
        x.~sema();
    }
}

static bool open_resource(ErlNifEnv *env) {
    auto flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    SEMA = enif_open_resource_type(env, nullptr, "sema", free_sema, flags, nullptr);
    return SEMA != nullptr;
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    if (!open_resource(env)) return -1;
    atom_ok = mk_atom(env, "ok");
    atom_error = mk_atom(env, "error");
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

    return res->try_to_take(env, pid);
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
    {"vacate", 2, vacate}
};

ERL_NIF_INIT(sema_nif, nif_funcs, &load, nullptr, nullptr, nullptr);
