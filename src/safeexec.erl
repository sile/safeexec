%% @copyright 2013-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Interface Module for safeexec command
-module(safeexec).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         open_port/2,
         get_safeexec_path/0
        ]).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec open_port(PortName, PortSettings) -> port() when
      PortName     :: {spawn, Command :: string() | binary()}
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
% unsupported:      | in
                    | out
                    | binary
                    | eof
                    | {parallelism, Boolean :: boolean()}
                    | hide.
open_port({spawn, Command}, PortSettings) when is_binary(Command) ->
    safeexec:open_port({spawn, binary_to_list(Command)}, PortSettings);
open_port({spawn, Command}, PortSettings) ->
    erlang:open_port({spawn, quote(get_safeexec_path()) ++ " " ++ Command}, PortSettings);
open_port({spawn_executable, FileName}, PortSettings0) ->
    PortSettings1 = update_setting(arg0, fun (Arg0) -> Arg0 end, get_safeexec_path(), PortSettings0),
    PortSettings2 = update_setting(args, fun (Args) -> [FileName | Args] end, [FileName], PortSettings1),
    _ = case lists:member(in, PortSettings2) of
            true  -> error({unsupported_option, in});  % inオプションがついているとポートが閉じたこと(= コマンド側の標準入力が閉じたこと)が検出できないので非サポート
            false -> ok
        end,
    erlang:open_port({spawn_executable, get_safeexec_path()}, PortSettings2).

-spec get_safeexec_path() -> string().
get_safeexec_path() ->
    PrivDir =
        case code:priv_dir(?MODULE) of
            {error, bad_name} -> filename:join(filename:dirname(filename:dirname(code:which(?MODULE))), "priv");
            Dir               -> Dir
        end,
    case os:find_executable("safeexec", PrivDir) of
        false -> error({safexec_command_is_not_found, PrivDir});
        Path  -> filename:absname(Path)
    end.

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
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
