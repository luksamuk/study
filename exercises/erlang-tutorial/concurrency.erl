-module(concurrency).
-export([proc/1, backandforth/1, loopingproc/1, loopingprocM/0,
	 loopingprocM/1, spawnProcessRing/3, processring/2,
	 starproc/0, stardispatch/2, stardispatchall/2,
	 starcreateproc/2, procsstar/2]).


proc(Num) ->
    receive
	{From, X} when X < Num ->
	    io:format("~p: Received ~p from ~p.~n",
		      [self(), X, From]),
	    From ! { self(), (X + 1) },
	    proc(Num);
	{From, Num} ->
	    From ! { self(), Num },
	    io:format("Process ~p finished gracefully.~n",
		      [self()])
    end.

% Starts two processes which communicate back and forth
% N times, then terminate gracefully.
backandforth(N) ->
    Proc1 = spawn(concurrency, proc, [N]),
    Proc2 = spawn(concurrency, proc, [N]),
    Proc1 ! {Proc2, 0}.

% ===========================================================


loopingproc(Pid) ->
    receive
	X when X > 0 ->
	    io:format("~p: Pushing ~p forward to ~p~n",
		      [self(), X, Pid]),
	    Pid ! X,
	    loopingproc(Pid);
	0 ->
	    io:format("~p: Finishing gracefully.~n",
		      [self()]),
	    Pid ! 0
end.

loopingprocM() ->
    io:format("~p (MASTER): Spawned~n", [self()]),
    receive
	{SendTo, X}  ->
	    io:format("~p (MASTER): Pushing ~p forward to ~p~n",
		      [self(), (X - 1), SendTo]),
	    SendTo ! (X - 1),
	    loopingprocM(SendTo)
end.

loopingprocM(SendTo) ->
    receive
	X when X > 0 ->
	    io:format("~p (MASTER): Pushing ~p forward to ~p~n",
		      [self(), (X - 1), SendTo]),
	    SendTo ! (X - 1),
	    loopingprocM(SendTo);
	0 ->
	    io:format("~p (MASTER): Finishing everything gracefully.~n",
		      [self()])
end.


spawnProcessRing(First, N, It) when It =/= N ->
    io:format("Spawning process #~p~n", [It]),
    spawn(concurrency,
	  loopingproc,
	  [spawnProcessRing(First, N, It + 1)]);
spawnProcessRing(First, N, N) ->
    io:format("Spawning last process #~p~n", [N]),
    spawn(concurrency,
	  loopingproc,
	  [First]).


% Spawns N processes and loops messages through them,
% then finishes the processes gracefully.
% N => Amount of processes
% M => Amount of full loops through all processes
processring(N, M) ->
    % Spawn master
    Master = spawn(concurrency, loopingprocM, []),
    % Spawn rest
    Rest = spawnProcessRing(Master, (N - 1), 0),
    io:format("Sending message~n"),
    Master ! {Rest, M}.


% ===================================================

starproc() ->
    receive
	{From, M} when M > 0 ->
	    io:format("~p: Received message ~p from ~p~n",
		      [self(), M, From]),
	    starproc();
	{From, 0} ->
	    io:format("~p: Received last message from ~p~n",
		      [self(), From])
end.


stardispatch([HPid | T], M) ->
    HPid ! {self(), M},
    stardispatch(T, M);
stardispatch([], _)  -> true.

stardispatchall(PidList, M) when M > 0 ->
    io:format("~p: Dispatching message #~p~n", [self(), M]),
    stardispatch(PidList, M),
    stardispatchall(PidList, M - 1);
stardispatchall(PidList, 0) ->
    io:format("~p: Dispatching last message~n", [self()]),
    stardispatch(PidList, 0),
    io:format("~p: Finishing gracefully.~n", [self()]).


starcreateproc(ProcList, N) when N > 0 ->
    starcreateproc(lists:append(ProcList, [spawn(concurrency, starproc, [])]),
		   N - 1);
starcreateproc(ProcList, 0) -> ProcList.


% Creates a star of N processes. Another process will send a message
% to all of them M times, and then all of them will exit gracefully.
% N => Amount of processes
% M => Amout of time the message is sent.
procsstar(N, M) ->
    % Create a process list
    io:format("Creating process list.~n"),
    ProcList = starcreateproc([], N),
    % Echo a message M times through the list
    io:format("Dispatching messages M times decrementally~n"),
    spawn(concurrency, stardispatchall, [ProcList, M]).
    
