-module(sema_nif).

-export([
    create/1,
    create/2,
    info/1,
    capacity/1,
    capacity/2,
    acquire/1,
    acquire/2,
    release/1,
    release/2,
    release/3
]).

-nifs([
    create/1,
    create/2,
    info/1,
    capacity/1,
    capacity/2,
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
-type sema_id() :: sema_ref() | atom().

-type sema_opts() :: #{name => atom()}.

-export_type([sema_ref/0, sema_id/0, sema_opts/0, acquire_ret/0, release_ret/0]).

% @doc Create a new semaphore with the given capacity
-spec create(Max :: pos_integer()) -> sema_ref().
create(_) -> not_loaded(?LINE).

% @doc Create a new semaphore with the given capacity
% `Opts' is a map of options:
% <ul>
% <li>`{name, Name::atom()}' semaphore name
% </ul>
-spec create(pos_integer(), sema_opts()) -> sema_ref().
create(Max, Opts) when is_integer(Max), is_map(Opts) -> not_loaded(?LINE).

% @doc Get internal properties of the semaphore resource
-spec info(Semaphore :: sema_id()) ->
    #{
        % number of units acquired
        cnt := non_neg_integer(),
        % number of monitored processes terminated
        dead := non_neg_integer(),
        % resource capacity
        max := non_neg_integer()
    }.
info(_) -> not_loaded(?LINE).

% @doc Get semaphore maximum capacity
-spec capacity(Semaphore :: sema_id()) -> pos_integer().
capacity(_) -> not_loaded(?LINE).

% @doc Set semaphore maximum capacity.
% Return old capacity.
-spec capacity(Semaphore :: sema_id(), Max :: integer()) -> pos_integer().
capacity(_, _Max) -> not_loaded(?LINE).

% @doc Acquire resource unit for calling process, monitor process
-spec acquire(Semaphore :: sema_id()) -> Ret :: acquire_ret().
acquire(_) -> not_loaded(?LINE).

% @doc Acquire resource Cnt units for calling process, monitor process
-spec acquire(Semaphore :: sema_id(), Cnt :: pos_integer()) -> Ret :: acquire_ret().
acquire(_, _) -> not_loaded(?LINE).

% @doc Release resource unit acquired by calling process
-spec release(Semaphore :: sema_id()) -> Ret :: release_ret().
release(_) -> not_loaded(?LINE).

% @doc Release resource unit(s) acquired by calling/another process
-spec release(Semaphore :: sema_id(), Arg :: pos_integer() | pid()) -> Ret :: release_ret().
release(_, _) -> not_loaded(?LINE).

% @doc Release resource units acquired by another process
-spec release(Semaphore :: sema_id(), Cnt :: pos_integer(), Pid :: pid()) -> Ret :: release_ret().
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
