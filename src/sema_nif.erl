-module(sema_nif).

-export([
    create/1,
    info/1,
    occupy/1,
    occupy/2,
    occupy/3,
    vacate/1,
    vacate/2
]).

-nifs([
    create/1,
    info/1,
    occupy/1,
    occupy/2,
    occupy/3,
    vacate/1,
    vacate/2
]).

-on_load(init/0).

-define(APPNAME, sema_nif).
-define(LIBNAME, sema_nif).

-export_type([sema_ref/0]).
-opaque sema_ref() :: reference().

-spec create(Max :: pos_integer()) -> sema_ref().
create(_) -> not_loaded(?LINE).

% get internal properties of the semaphore resource
-spec info(Semaphore :: sema_ref()) ->
    #{
        % number of units acquired
        cnt := non_neg_integer(),
        % number of monitored processes terminated
        dead := non_neg_integer(),
        % resource capacity
        max := non_neg_integer()
    }.
info(_) -> not_loaded(?LINE).

-type occupy_ret() ::
    % process got resource unit, return number of units acquired so far
    {ok, N :: pos_integer()}
    % process already acquired the resource
    | {error, duplicate_pid}
    % process already died
    | {error, not_found}
    % no resource units available
    | {error, backlog_full}.

% acquire resource unit for calling process, monitor process
-spec occupy(Semaphore :: sema_ref()) -> Ret :: occupy_ret().
occupy(_) -> not_loaded(?LINE).

% acquire resource unit for process Pid, don't monitor process
-spec occupy(Semaphore :: sema_ref(), Pid :: pid()) -> Ret :: occupy_ret().
occupy(_, _) -> not_loaded(?LINE).

% acquire resource unit for process Pid, optionally monitor process
-spec occupy(Semaphore :: sema_ref(), Pid :: pid(), Monitor :: boolean()) ->
    Ret :: occupy_ret().
occupy(_, _, _) -> not_loaded(?LINE).

-type vacate_ret() ::
    % process freed resource unit, return number of units acquired after that
    {ok, N :: pos_integer()}
    % no process that holds resource found
    | {error, not_found}.

% release resource unit acquired by calling process
-spec vacate(Semaphore :: sema_ref()) -> Ret :: vacate_ret().
vacate(_) -> not_loaded(?LINE).

% release resource unit acquired by given process
-spec vacate(Semaphore :: sema_ref(), Pid :: pid()) -> Ret :: vacate_ret().
vacate(_, _) -> not_loaded(?LINE).

% internal
init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

% internal
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
