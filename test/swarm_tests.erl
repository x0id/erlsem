-module(swarm_tests).

-include_lib("eunit/include/eunit.hrl").

basic_swarm_test_() ->
    {timeout, 60_000, fun basic_swarm/0}.

basic_swarm() ->
    S = sema_nif:create(10_000),
    ?assertEqual(#{cnt => 0, dead => 0, max => 10_000}, sema_nif:info(S)),

    Self = self(),
    Pids = [
        spawn(fun() ->
            ?assertMatch({ok, _}, sema_nif:acquire(S)),
            Self ! {pass, self(), S},
            receive
                stop -> ok
            end
        end)
     || _ <- lists:seq(1, 10_000)
    ],

    loop(Pids),
    timer:sleep(100),

    ?assertEqual(#{cnt => 0, dead => 0, max => 10_000}, sema_nif:info(S)).

loop([]) ->
    ok;
loop([Pid | T]) ->
    receive
        {pass, Pid, S} ->
            spawn(fun() ->
                timer:sleep(1),
                ?assertMatch({ok, _}, sema_nif:release(S, Pid)),
                Pid ! stop
            end),
            loop(T)
    end.

seq_test_() ->
    rand:seed(default),
    {setup, fun() -> {rand:export_seed(), spawn(fun server/0)} end,
        fun({_State, Server}) -> Server ! stop end, fun(State) ->
            [
                test_seq_(State, Type, BacklogSize, UseServer, SleepMs)
             || {UseServer, SleepMs} <- [{false, 1}, {true, 0}],
                BacklogSize <- [10, 25, 50, 75, 100],
                Type <- [nif, ets]
            ]
        end}.

test_seq_({State, Server}, Type, BacklogSize, UseServer, SleepMs) ->
    {timeout, 60_000, fun() ->
        rand:seed(State),
        test_seq(Type, BacklogSize, Server, UseServer, SleepMs)
    end}.

test_seq(Type, BacklogSize, Server, UseServer, SleepMs) ->
    CRef = counters:new(2, []),
    BLog = init_backlog(Type, BacklogSize),
    ProcFun = proc_srv_fun(UseServer, Server, SleepMs),
    ReportFun = report(CRef),
    WorkerFun = worker(Type, BacklogSize, BLog, ProcFun, ReportFun),
    PidList = spawn_seq(WorkerFun, 25, 100, 500),
    wait_loop(PidList),
    io:format(user, " ~p|~p|~s|~pms> ~p ok, ~p errs ", [
        Type,
        BacklogSize,
        use_server_str(UseServer),
        SleepMs,
        counters:get(CRef, 1),
        counters:get(CRef, 2)
    ]),
    destroy_backlog(Type, BLog).

use_server_str(true) -> "server";
use_server_str(false) -> "direct".

proc(0, ReleaseF) ->
    ReleaseF();
proc(SleepN, ReleaseF) ->
    timer:sleep(SleepN),
    ReleaseF().

proc_srv_fun(false, _, SleepMs) ->
    fun(ReleaseF) -> proc(SleepMs, ReleaseF) end;
proc_srv_fun(true, Server, SleepMs) ->
    fun(ReleaseF) ->
        Server ! {request, SleepMs, ReleaseF, self()},
        receive
            {reply, ok} ->
                ok
        end
    end.

server() ->
    receive
        {request, SleepN, ReleaseF, Client} ->
            proc(SleepN, ReleaseF),
            Client ! {reply, ok},
            server();
        stop ->
            ok
    end.

report(CRef) ->
    Pid = self(),
    fun
        (ok) ->
            counters:add(CRef, 1, 1);
        (error) ->
            counters:add(CRef, 2, 1);
        (stop) ->
            Pid ! {stop, self()}
    end.

init_backlog(nif, Max) ->
    init_backlog(ets, Max),
    sema_nif:create(Max);
init_backlog(ets, _) ->
    ets:new(backlog, [
        named_table,
        public,
        {write_concurrency, true},
        {decentralized_counters, true}
    ]),
    ets:insert(backlog, {key, 0}),
    backlog.

destroy_backlog(nif, _) ->
    ets:delete(backlog),
    ok;
destroy_backlog(ets, backlog) ->
    ets:delete(backlog).

wait_loop([]) ->
    ok;
wait_loop([Pid | T]) ->
    receive
        {stop, Pid} ->
            ok
    end,
    wait_loop(T).

worker(nif, _, S, ProcF, ReportF) ->
    fun() ->
        nif_worker(S, ProcF, ReportF, 1),
        ReportF(stop)
    end;
worker(ets, Max, _, ProcF, ReportF) ->
    fun() ->
        ets_worker(Max, ProcF, ReportF, 1),
        ReportF(stop)
    end.

nif_worker(S, ProcF, ReportF, N) ->
    case sema_nif:acquire(S) of
        {ok, _} ->
            ReportF(ok),
            Pid = self(),
            ProcF(fun() -> sema_nif:release(S, Pid) end);
        {error, full} when N > 1 ->
            erlang:yield(),
            nif_worker(S, ProcF, ReportF, N - 1);
        {error, full} ->
            ReportF(error)
    end.

ets_worker(Max, ProcF, ReportF, N) ->
    case ets:update_counter(backlog, key, [{2, 0}, {2, 1, Max, Max}]) of
        [Max, Max] when N > 1 ->
            erlang:yield(),
            ets_worker(Max, ProcF, ReportF, N - 1);
        [Max, Max] ->
            ReportF(error);
        _ ->
            ReportF(ok),
            ProcF(fun() -> ets:update_counter(backlog, key, {2, -1, 0, 0}) end)
    end.

spawn_seq(Fun, Min, Max, SeqN) ->
    lists:concat([
        begin
            timer:sleep(1),
            spawn_par(Fun, rand(Min, Max))
        end
     || _ <- lists:seq(1, SeqN)
    ]).

spawn_par(Fun, N) ->
    [spawn(Fun) || _ <- lists:seq(1, N)].

rand(Min, Max) ->
    Min - 1 + rand:uniform(Max - Min + 1).
