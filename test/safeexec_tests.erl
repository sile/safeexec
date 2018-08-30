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
      {timeout, 60,
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
                             end, lists:seq(1, 3000))
       end}}
    ].
