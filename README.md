erlsem
=====

An OTP library implementing counting non-blocking semaphore with the ability to
monitor a process that obtains a lock and to release the corresponding
resource(s) automatically when the process exits.

Build
-----

```shell
$ rebar3 compile
$ rebar3 eunit
```

Demo
----

```erlang
Eshell V13.1.3  (abort with ^G)
1> Pid = self().
<0.145.0>
2> S = sema_nif:create(3).
#Ref<0.1541145699.2864316417.117910>
3> L = [spawn(fun() -> Pid ! {self(), sema_nif:acquire(S)}, timer:sleep(10) end) || _ <- lists:seq(1, 5)].
[<0.154.0>,<0.155.0>,<0.156.0>,<0.157.0>,<0.158.0>]
4> [receive {P, X} -> X end || P <- L].
[{ok,1},
 {ok,2},
 {ok,3},
 {error,full},
 {error,full}]
5> sema_nif:create(sema, 5).
#Ref<0.1541145699.2864316445.123450>
6> sema_nif:acquire(sema).
{ok, 1}
6> sema_nif:release(sema).
{ok, 0}
```
