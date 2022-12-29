-module(sema_nif).

-export([create/1, occupy/2, vacate/2]).

-nifs([create/1, occupy/2, vacate/2]).

-on_load(init/0).

-define(APPNAME, sema_nif).
-define(LIBNAME, sema_nif).

create(_) -> not_loaded(?LINE).

occupy(_, _) -> not_loaded(?LINE).

vacate(_, _) -> not_loaded(?LINE).

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

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
