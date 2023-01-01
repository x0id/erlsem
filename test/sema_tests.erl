-module(sema_tests).

-include_lib("eunit/include/eunit.hrl").

basic_api_test() ->
    ?assertError(badarg, sema_nif:create(-1)),

    S = sema_nif:create(3),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    Pid = self(),
    ?assertEqual({ok, 1}, sema_nif:occupy(S, Pid)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({error, duplicate_pid}, sema_nif:occupy(S, Pid)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:vacate(S, Pid)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({error, not_found}, sema_nif:vacate(S, Pid)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 1}, sema_nif:occupy(S, Pid)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({error, duplicate_pid}, sema_nif:occupy(S, Pid)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:vacate(S, Pid)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 1}, sema_nif:occupy(S)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:vacate(S)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ok.

parallel_test() ->
    ?assertError(badarg, test_parallel(-1, 100)),
    test_parallel(0, 100),
    test_parallel(1, 100),
    test_parallel(2, 100),
    test_parallel(20, 100),
    test_parallel(200, 100),
    test_parallel(200, 200),
    test_parallel(200, 500),
    test_parallel(200, 1500),
    ok.

test_parallel(BacklogSize, ProcessCount) ->
    S = sema_nif:create(BacklogSize),
    Top = self(),
    F = fun() ->
        Pid = self(),
        case sema_nif:occupy(S, Pid) of
            {ok, N} when is_integer(N), N > 0, N =< BacklogSize ->
                Top ! {tenant, Pid, N},
                receive
                    leave ->
                        {ok, Left} = sema_nif:vacate(S, Pid),
                        Top ! {left, Pid, Left}
                end;
            {error, backlog_full} ->
                Top ! {no_luck, Pid}
        end
    end,

    ProcPids = [spawn(F) || _ <- lists:seq(1, ProcessCount)],

    Tenants = gather_tenants(ProcPids, []),

    N1 = min(ProcessCount, BacklogSize),
    N1 = length(Tenants),

    % list of numbers showing the current backlog length
    % after tenant addition, as reported by tenants
    L1 = lists:usort([N || {_, N} <- Tenants]),
    L1 = lists:seq(1, N1),

    TenantPids = [Pid || {Pid, _} <- Tenants],
    [Pid ! leave || Pid <- TenantPids],

    % list of numbers showing the current backlog length
    % after tenant removing, as reported by tenants
    L2 = lists:usort(wait_tenants(TenantPids, [])),
    L2 = lists:seq(0, N1 - 1),

    ok.

gather_tenants([], Acc) ->
    Acc;
gather_tenants(Pids, Acc) ->
    receive
        {tenant, Pid, N} ->
            gather_tenants(Pids -- [Pid], [{Pid, N} | Acc]);
        {no_luck, Pid} ->
            gather_tenants(Pids -- [Pid], Acc)
    end.

wait_tenants([], Acc) ->
    Acc;
wait_tenants(Pids, Acc) ->
    receive
        {left, Pid, N} ->
            wait_tenants(Pids -- [Pid], [N | Acc])
    end.

sample_race_test() ->
    race_loop(4000, 50, fun atomic_ops/1),
    ok.

atomic_ops(N) ->
    A = atomics:new(1, []),
    Pid = spawn(fun() -> atomics:add(A, 1, 1) end),
    spin(N),
    exit(Pid, kill),
    1 == atomics:get(A, 1).

sema_race_test_() -> {timeout, 60, fun test_sema_race/0}.

test_sema_race() ->
    ?assertNotException(
        throw,
        {error, incomplete},
        race_loop(4000, 1000000, fun sema_ops/1)
    ).

sema_ops(N) ->
    S = sema_nif:create(2),
    Pid = spawn(fun() -> sema_nif:occupy(S, self()) end),
    spin(N),
    exit(Pid, kill),
    _ = sema_nif:info(S),
    case sema_nif:occupy(S, Pid) of
        {error, duplicate_pid} ->
            % occupation by Pid is done in full, reduce kill delay
            true;
        {ok, 1} ->
            % occupation by Pid is not done, increase kill delay
            false;
        {ok, 2} ->
            % this may happen due to race, double check it after a delay
            timer:sleep(1),
            % receive {'DOWN', Mref, process, Pid, _Info} -> ok end,
            case sema_nif:vacate(S, Pid) of
                {ok, 0} ->
                    true;
                {ok, 1} ->
                    % occupation was partally done, Pid was not registered!
                    throw({error, incomplete});
                Else ->
                    throw({error, Else})
            end
    end.

sema_gc_race_test_() -> {timeout, 60, fun test_sema_gc_race/0}.

test_sema_gc_race() ->
    race_loop(4_000, 1_000_000, fun sema_ops_gc/1).

% version with automatic dead processes collection
sema_ops_gc(N) ->
    S = sema_nif:create(1),
    % acquire and monitor
    Pid = spawn(fun() -> sema_nif:occupy(S) end),
    spin(N),
    exit(Pid, kill),
    wait_loop(S, 10, 100_000).

wait_loop(_, _, 0) ->
    % occupation was partally done, Pid was not registered!
    throw({error, incomplete});
wait_loop(_, 0, _) ->
    % occupation by Pid is not done, increase kill delay
    false;
wait_loop(S, N1, N2) ->
    case sema_nif:info(S) of
        #{cnt := 0, dead := 0} ->
            % occupation by Pid is not yet done, or, it's done, but
            % dead counter it not yet updated, try couple more times
            wait_loop(S, N1 - 1, N2);
        #{cnt := 0, dead := 1} ->
            % acquire/release is done in full, reduce kill delay
            true;
        #{cnt := 1, dead := 0} ->
            % occupation is partally done, Pid is not yet registered
            % keep checking it again until success or timeout
            wait_loop(S, N1, N2 - 1)
    end.

race_loop(SpinCount, IterationsLeft, Fun) ->
    race_loop(SpinCount, Fun(SpinCount), IterationsLeft, Fun).

race_loop(SpinCount, Result, IterationsLeft, Fun) ->
    % io:format(user, "~p ~p ~p~n", [IterationsLeft, SpinCount, Result]),
    if
        IterationsLeft > 0 ->
            N_ = adjust_spin(Result, SpinCount),
            race_loop(N_, Fun(N_), IterationsLeft - 1, Fun);
        true ->
            ok
    end.

adjust_spin(true, SpinCount) -> SpinCount - pace(SpinCount);
adjust_spin(false, SpinCount) -> SpinCount + pace(SpinCount).

pace(SpinCount) ->
    X = SpinCount div 10,
    X + rand:uniform(X).

spin(0) ->
    ok;
spin(SpinCount) ->
    spin(SpinCount - 1).
