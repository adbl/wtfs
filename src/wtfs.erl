-module(wtfs).
-export([start/2, stop/0, init/1]).

start(Path, SharedLib) ->
    case erl_ddll:try_load(Path, SharedLib, [{driver_options,[kill_ports]}]) of
	{ok, loaded} -> ok;
	{ok, already_loaded} -> ok;
	Error -> exit(Error)
    end,
    spawn(?MODULE, init, [SharedLib]).

init(SharedLib) ->
    register(complex, self()),
    Port = open_port({spawn_driver, SharedLib}, []),
    loop(Port).

stop() ->
    complex ! stop.

loop(Port) ->
    receive
        {command, Msg} -> Port ! {self(), {command, Msg}};
	%% {call, Caller, Msg} ->
	%%     Port ! {self(), {command, encode(Msg)}},
	%%     receive
	%% 	{Port, {data, Data}} ->
	%% 	    Caller ! {complex, decode(Data)}
	%%     end,
	%%     loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated);
        Data -> io:format("data: ~p~n", Data)
    end.
