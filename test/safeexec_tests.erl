%% @copyright 2013-2015 Takeru Ohta <phjgt308@gmail.com>
-module(safeexec_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------------------
%% Unit Tests
%%--------------------------------------------------------------------------------
%% If the port program closes its stdout without exiting,
%%  the exit_status option will not work.
%% {Port, {exit_status, Status}}
open_port_test_() ->
    [
     {"unix kill(SIGKILL)",
      fun () ->
              lists:foreach(fun (I) ->
                                    Port = safeexec:open_port({spawn_executable, "/bin/sleep"}, [{args, ["100"]}, exit_status]),
                                    OsPid = proplists:get_value(os_pid, erlang:port_info(Port)),
                                    os:cmd("kill -9 " ++ integer_to_list(OsPid)),
                                    receive
                                        {Port, {exit_status, _}} -> ?assert(true)
                                    after 500 ->
                                            ?debugVal(I),
                                            ?debugVal(erlang:port_info(Port)),
                                            ?assert(false)
                                    end
                            end, lists:seq(1, 500))
      end}
    ].

make_safeexec_options_test_() ->
    [
     {"assign_cpu_id_set_test",
      fun () ->
              Os = case os:type() of
                       {_, linux} -> linux;
                       _ -> other_os
                   end,
              
              try safeexec:make_safeexec_options([{assign_cpu_id_set, [0, {1, 3}, {3, 4}, 6]}]) of
                  {Options2, Result} ->
                      ?assertMatch(Os, linux),
                      ?assertMatch(Result, ["--cpu", "0x5F"]),
                      ?assertMatch(Options2, [])
              catch
                  %% linux以外でassign_cpu_id_set optionは指定できない
                  error:assign_cpu_id_set_is_only_used_for_linux ->
                      ?assertMatch(Os, other_os)
              end
      end}
    ].
