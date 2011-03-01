-module(wtfs).
-export([start/1, init/1]).

-define(DRIVER, "wtfs_drv").

start(Mountpoint) ->
    case erl_ddll:try_load(code:priv_dir(wtfs), ?DRIVER,
                           [{driver_options,[kill_ports]}]) of
	{ok, loaded} -> ok;
	{ok, already_loaded} -> ok;
	Error -> exit(Error)
    end,
    spawn(?MODULE, init, [Mountpoint]).

init(Mountpoint) ->
    Port = open_port({spawn_driver, ?DRIVER ++ " " ++ Mountpoint}, []),
    loop(Port).

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
