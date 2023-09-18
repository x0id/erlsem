-module(sema_tests).

-include_lib("eunit/include/eunit.hrl").

basic_api_test() ->
    ?assertError(badarg, sema_nif:create(-1)),

    S = sema_nif:create(3),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 1}, sema_nif:acquire(S)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 2}, sema_nif:acquire(S)),
    ?assertEqual(#{cnt => 2, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 3}, sema_nif:acquire(S)),
    ?assertEqual(#{cnt => 3, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({error, full}, sema_nif:acquire(S)),
    ?assertEqual(#{cnt => 3, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 2}, sema_nif:release(S)),
    ?assertEqual(#{cnt => 2, dead => 0, max => 3}, sema_nif:info(S)),

    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual({error, not_found}, sema_nif:release(S, Pid)),
    ?assertEqual(#{cnt => 2, dead => 0, max => 3}, sema_nif:info(S)),
    Pid ! stop,

    ?assertEqual({ok, 1}, sema_nif:release(S)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:release(S)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({error, not_found}, sema_nif:release(S)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 1}, sema_nif:acquire(S)),
    ?assertEqual(#{cnt => 1, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:release(S, self())),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 3}, sema_nif:acquire(S, 3)),
    ?assertEqual(#{cnt => 3, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 2}, sema_nif:release(S, self())),
    ?assertEqual(#{cnt => 2, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:release(S, 2, self())),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 3}, sema_nif:acquire(S, 3)),
    ?assertEqual(#{cnt => 3, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 2}, sema_nif:release(S)),
    ?assertEqual(#{cnt => 2, dead => 0, max => 3}, sema_nif:info(S)),

    ?assertEqual({ok, 0}, sema_nif:release(S, 2)),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    ok.

gc_test() ->
    S = sema_nif:create(3),
    ?assertEqual(#{cnt => 0, dead => 0, max => 3}, sema_nif:info(S)),

    Self = self(),
    {Pid, Mref} = spawn_monitor(fun() ->
        ?assertEqual({ok, 1}, sema_nif:acquire(S)),
        ?assertEqual({ok, 3}, sema_nif:acquire(S, 2)),
        Self ! ready,
        receive
            stop -> ok
        end
    end),
    receive
        ready -> ok
    end,
    ?assertEqual(#{cnt => 3, dead => 0, max => 3}, sema_nif:info(S)),

    Pid ! stop,
    receive
        {'DOWN', Mref, process, Pid, _Info} -> ok
    end,
    ?assertEqual(#{cnt => 0, dead => 1, max => 3}, sema_nif:info(S)),

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
        case sema_nif:acquire(S) of
            {ok, N} when is_integer(N), N > 0, N =< BacklogSize ->
                Top ! {tenant, Pid, N},
                receive
                    leave ->
                        {ok, Left} = sema_nif:release(S),
                        Top ! {left, Pid, Left}
                end;
            {error, full} ->
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
    race_loop(4_000, 1_000_000, fun sema_ops/1).

sema_ops(N) ->
    S = sema_nif:create(2),
    {Pid, Mref} = spawn_monitor(fun() ->
        sema_nif:acquire(S),
        receive
            x -> x
        end
    end),
    spin(N),
    exit(Pid, kill),
    case sema_nif:acquire(S) of
        % early kill
        {ok, 1} ->
            % occupation by Pid is not done, increase kill delay
            false;
        % late kill - S is fully occupied, will have to reduce delay
        {ok, 2} ->
            % ensure process died
            receive
                {'DOWN', Mref, process, Pid, _Info} -> ok
            end,
            % ensure nif handled monitor
            ok = wait_dead(1_000_000, S),
            % ensure only one unit occupied now
            {ok, 0} = sema_nif:release(S),
            true
    end.

wait_dead(0, _) ->
    alive;
wait_dead(N, S) ->
    case sema_nif:info(S) of
        #{dead := 1} ->
            ok;
        _ ->
            wait_dead(N - 1, S)
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
