erlsem
=====

An OTP library implementing counting non-blocking semaphore with the ability to monitor a process that obtains a lock and to release the corresponding resource(s) automatically in case the process exits.


Build
-----

    $ rebar3 compile
    $ rebar3 eunit

Demo
----

    Eshell V13.1.3  (abort with ^G)
    1> Pid = self().
    <0.145.0>
    2> S = sema_nif:create(3).
    #Ref<0.1541145699.2864316417.117910>
    3> L = [spawn(fun() -> Pid ! {self(), sema_nif:occupy(S)}, timer:sleep(10) end) || _ <- lists:seq(1, 5)].
    [<0.154.0>,<0.155.0>,<0.156.0>,<0.157.0>,<0.158.0>]
    4> [receive {P, X} -> X end || P <- L].
    [{ok,1},
     {ok,2},
     {ok,3},
     {error,backlog_full},
     {error,backlog_full}]
