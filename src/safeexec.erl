%% @copyright 2013-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Interface Module for safeexec command
-module(safeexec).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         open_port/2,
         get_safeexec_path/0,
         make_safeexec_options/1 %% only for unit test
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
                    | hide
%% safeexec固有オプション                      
                    | {assign_cpu_id_set, [CpuId | CpuIdRange]}, %% only used for linux
      CpuId         :: non_neg_integer(),
      CpuIdRange    :: {FromCpuId, ToCpuId},
      FromCpuId     :: CpuId,
      ToCpuId       :: CpuId.      
      
open_port({spawn, Command}, PortSettings) when is_binary(Command) ->
    safeexec:open_port({spawn, binary_to_list(Command)}, PortSettings);
open_port({spawn, Command}, PortSettings) ->
    {PortSettings2, SafeExecOptions} = make_safeexec_options(PortSettings),
    erlang:open_port({spawn, quote(get_safeexec_path()) ++ " " ++ string:join((SafeExecOptions ++ [Command]), " ")}, PortSettings2);
open_port({spawn_executable, FileName}, PortSettings0) ->
    {PortSettings1, SafeExecOptions} = make_safeexec_options(PortSettings0),
    PortSettings2 = update_setting(arg0, fun (Arg0) -> Arg0 end, get_safeexec_path(), PortSettings1),
    PortSettings3 = update_setting(args, fun (Args) -> SafeExecOptions ++ [FileName] ++ Args end, SafeExecOptions ++ [FileName], PortSettings2),
    erlang:open_port({spawn_executable, get_safeexec_path()}, PortSettings3).

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

-spec update_setting(term(), function(), term(), [{term(), term()} | term()]) -> [{term(), term()} | term()].
update_setting(Key, UpdateFun, _InitialValue, [{Key, Value} | Settings]) ->
    [{Key, UpdateFun(Value)} | Settings];
update_setting(Key, UpdateFun, InitialValue, [Setting | Settings]) ->
    [Setting | update_setting(Key, UpdateFun, InitialValue, Settings)];
update_setting(Key, _UpdateFun, InitialValue, []) ->
    [{Key, InitialValue}].

%% @doc PortSettingsからsafeexec固有の値を取り出し, safeexec binaryへ渡すオプションリストを生成する.
-spec make_safeexec_options(PortSettings::[{atom(), term()} | atom()]) -> {LeftPortSettings::[{atom(), term()} | atom()], SafeExecOptions::[string()]}.
make_safeexec_options(PortSettings) ->
    lists:foldl(fun(F, {Settings, Options}) ->
                        {Settings2, NewOption} = F(Settings),
                        {Settings2, NewOption ++ Options}
                end,
                {PortSettings, []},
                %% option処理関数群
                [
                 fun process_assign_cpu_id_set/1
                ]).

-spec process_assign_cpu_id_set(Settings::[{atom(), term()} | atom()]) ->
                                       {Settings::[{atom(), term()} | atom()], AssignCpuIdSetOptions::[string()]}.
process_assign_cpu_id_set(Settings) ->
    case lists:keytake(assign_cpu_id_set, 1, Settings) of
        false -> {Settings, []};
        {value, {_, Value}, Settings2} ->
            case os:type() of
                %% only used for linux
                {_, linux} ->
                    Mask = to_bitmask(Value),
                    {Settings2, ["--cpu", "0x" ++ integer_to_list(Mask, 16)]}; %% bitmaskを16進記法で出力
                _ ->
                    erlang:error(assign_cpu_id_set_is_only_used_for_linux)
            end
    end.

to_bitmask(AssignCpuIdSetSpecs) ->
    lists:foldl(fun(Spec, Mask) ->
                        case Spec of
                            %% [from, to]番目のbitを立てる
                            {From, To} ->
                                set_bit(Mask, {From, To});
                            %% N番目のbitを立てる
                            N ->
                                set_bit(Mask, N)
                        end
                end,
                0,
                AssignCpuIdSetSpecs).

set_bit(Mask, {From, To}) ->
    Mask bor (((1 bsl (To + 1)) - 1) - ((1 bsl From) - 1));
set_bit(Mask, N) ->
    Mask bor (1 bsl N).
