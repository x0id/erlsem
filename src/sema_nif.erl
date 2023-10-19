-module(sema_nif).

-export([
    create/1,
    info/1,
    acquire/1,
    acquire/2,
    release/1,
    release/2,
    release/3
]).

-nifs([
    create/1,
    info/1,
    acquire/1,
    acquire/2,
    release/1,
    release/2,
    release/3
]).

-on_load(init/0).

-define(APPNAME, sema_nif).
-define(LIBNAME, sema_nif).

-type acquire_ret() ::
    % process got resource unit, return number of units acquired so far
    {ok, N :: pos_integer()}
    % no resource units available
    | {error, full}.

-type release_ret() ::
    % process freed resource unit, return number of units acquired after that
    {ok, N :: pos_integer()}
    % no process that holds resource found
    | {error, not_found}.

-type sema_ref() :: reference().

-export_type([sema_ref/0, acquire_ret/0, release_ret/0]).

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

% acquire resource unit for calling process, monitor process
-spec acquire(Semaphore :: sema_ref()) -> Ret :: acquire_ret().
acquire(_) -> not_loaded(?LINE).

% acquire resource Cnt units for calling process, monitor process
-spec acquire(Semaphore :: sema_ref(), Cnt :: pos_integer()) -> Ret :: acquire_ret().
acquire(_, _) -> not_loaded(?LINE).

% release resource unit acquired by calling process
-spec release(Semaphore :: sema_ref()) -> Ret :: release_ret().
release(_) -> not_loaded(?LINE).

% release resource unit(s) acquired by calling/another process
-spec release(Semaphore :: sema_ref(), Arg :: pos_integer() | pid()) -> Ret :: release_ret().
release(_, _) -> not_loaded(?LINE).

% release resource units acquired by another process
-spec release(Semaphore :: sema_ref(), Cnt :: pos_integer(), Pid :: pid()) -> Ret :: release_ret().
release(_, _, _) -> not_loaded(?LINE).

% internal
init() ->
    SoName =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join([AppPath, "priv", ?LIBNAME]);
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

% internal
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
