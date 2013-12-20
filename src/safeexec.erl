%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Interface Module for safeexec command
-module(safeexec).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         open_port/2
        ]).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec open_port(PortName, PortSettings) -> port() when
      PortName     :: {spawn, Command :: string()}
                    | {spawn_executable, FileName :: file:name()},
      PortSettings :: [Opt],
      Opt          :: {packet, N :: 1 | 2 | 4}
                    | stream
                    | {line, L :: non_neg_integer()}
                    | {cd, Dir :: string()}
                    | {env, Env :: [{Name :: string(), Val :: string() | false}]}
                    | {args, [string() | binary()]}
                    | {arg0, string() | binary()}
                    | exit_status
                    | use_stdio
                    | nouse_stdio
                    | stderr_to_stdout
                    | in
                    | out
                    | binary
                    | eof
                    | {parallelism, Boolean :: boolean()}
                    | hide.
open_port({spawn, Command}, PortSettings) ->
    erlang:open_port({spawn, quote(get_safeexec_path()) ++ " " ++ Command}, PortSettings);
open_port({spawn_executable, FileName}, PortSettings0) ->
    PortSettings1 = update_setting(arg0, fun (Arg0) -> Arg0 end, get_safeexec_path(), PortSettings0),
    PortSettings2 = update_setting(args, fun (Args) -> [FileName | Args] end, [FileName], PortSettings1),
    erlang:open_port({spawn_executable, get_safeexec_path()}, PortSettings2).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec get_safeexec_path() -> string().
get_safeexec_path() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} -> error({priv_dir_is_not_found, ?MODULE});
        PrivDir           ->
            case os:find_executable("safeexec", PrivDir) of
                false -> error({safexec_command_is_not_found, PrivDir});
                Path  -> filename:absname(Path)
            end
    end.

-spec quote(string()) -> string().
quote([])        -> [];
quote([$\' | S]) -> "'\\''" ++ quote(S);
quote([C   | S]) -> [C | quote(S)].

-spec update_setting(term(), function(), term(), [{term(), term()}]) -> [{term(), term()}].
update_setting(Key, UpdateFun, InitialValue, Settings) ->
    case lists:keytake(Key, 1, Settings) of
        false                          -> [{Key, InitialValue}     | Settings];
        {value, {_, Value}, Settings2} -> [{Key, UpdateFun(Value)} | Settings2]
    end.
