-module(bel3).
-compile(export_all).

% zum debuggenn
% compile:file("bel3.erl",[debug_info])

ut()->eunit:test(bel3_tests).

print (P,Q)->io:write(P),io:fwrite(" "),io:write(Q),io:fwrite("~n"),true.
print(P)->io:write(P),io:fwrite("~n"),true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Algorithmus fuer die verteilte Berechnung Magischer Quadrate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Teil 1 - Berechnung magischer Quadrate auf einem Rechner  %%%%

% Berechnet alle moeglichen Zeilen eines Magischen Quadrats
% Aufruf: row(Max, Value) - z.B. row(3,15,lists:seq(1,15))
% Max - Seitengroesse des Quadrats
% Value - Wert der Summe der Zeile
% Elements - Elemente aus denen ausgewaehlt werden soll
-spec row(non_neg_integer(), non_neg_integer(),list(non_neg_integer())) -> list(list(non_neg_integer())).
row(0, Value, Elements) ->[[]];
row(Max, Value, Elements) ->  [Y || Y <- comb(Elements, Max, Max, Value)].
% row(Max, Value, Elements) ->  [Y || Y <- comb(Elements, Max), lists:sum(Y) == Value].
% row(Max, Value, Elements) ->  [Y || Y <- comb(Elements, Max), lists:sum(Y) == Value, hasDoubles(Y)].

comb(_,0, _, _) -> [ [] ];
comb(Elements, Count, Max, Value) ->
  [[Number | Liste] || Liste <- comb(Elements, Count - 1, Max, Value),   Number <- Elements, not(lists:member(Number, Liste)),
  not((Count == Max) andalso (lists:sum([Number | Liste]) /= Value))].

hasDoubles(List)-> length(sets:to_list(sets:from_list(List))) /= length(List).




% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
% Aufruf duplicate(Liste1,Liste2)
% Liste1 - Erste Liste
% Liste2 - Zweite Liste
-spec duplicate(list(non_neg_integer()),list(non_neg_integer())) -> true | false.
duplicate2([],[])->false;
duplicate2(Liste1,Liste2 )-> S1 = sets:from_list(Liste1),
                            S2 = sets:from_list(Liste2),
                            U = sets:intersection(S1,S2),
                            sets:size(U) /= 0.

% viel schneller
duplicate(Liste1, Liste2) -> lists:any(fun (E) -> lists:member(E, Liste2) end, Liste1).

% combineRows setzt eine beliebige Anzahl von Reihen, die vorab berechnet werden, zusammen
% Dabei wird ueberprueft, ob sich doppelte Elemente innerhalb der Reihen befinden.
% Aufruf: combineRows (Col, Max, Value)
% Col - Anzahl der Reihen, die berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile
% Value - Wert der Summe der Zeile
% Elems - Elemente aus denen gewaehlt werden soll

-spec combineRows(non_neg_integer(), non_neg_integer(), non_neg_integer(), list(non_neg_integer()))->list(list(non_neg_integer())).
combineRows(Col, Max, Value) -> combineRows(Col, Max, Value,lists:seq(1,Max*Max)).

combineRows(Col,Max,Value,Elems)->cr(Col,row(Max,Value,Elems)).

cr(0,_)->[[]];
cr(X, Rows)->[ Y++Q || Y <-  cr(X-1,Rows), Q <- Rows, not(duplicate(Y, Q))].


% calcSquares berechnet aus einem Teilquadrat alle moeglichen gueltigen Quadrate, die sich bilden lassen
% Aufruf: calcSquares(Part, Max, Value)
% Part - Teilquadrat fuer das die Magischen Quadrate berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile/Spalte
% Value - Wert der Summe einer Zeile
-spec calcSquares(list(non_neg_integer()), non_neg_integer(), non_neg_integer()) -> list(list(non_neg_integer())).
%calcSquares(Part, Max, MagicNumber)-> [[Y|Q] || Y <- [Part], Q <- combineRows(Max - length(Part)/Max,Max,Value, lists:seq(1,Max*Max)),duplicate(lists:flatten(Y),Q)==false].


calcSquares(Part, Max, MagicNumber)-> Rows = combineRows(trunc(Max - length(Part)/Max - 1), Max,MagicNumber,lists:seq(1,Max*Max)),
                                        Candidates = [Part ++ Y || Y <- Rows, false == duplicate(Y, Part)],
                                        [C++FinalRow || C <- Candidates, FinalRow <- [getLastRow(C, Max, MagicNumber)], 
                                    lists:sum(FinalRow) == MagicNumber 
                                    andalso not(duplicate(FinalRow, C)) 
                                    andalso length(FinalRow)==Max 
                                    andalso sumDiagonalLR(C++FinalRow,Max,1) == MagicNumber 
                                    andalso sumDiagonalRL(C++FinalRow,Max,Max) == MagicNumber
                                    andalso hasDoubles(FinalRow) == false].



% \                                     
sumDiagonalLR(_,Max,Step) when Max < Step -> 0;
sumDiagonalLR(Matrix, Max, Step)-> getCell(Matrix, Step, Step, Max) + sumDiagonalLR(Matrix, Max, Step +1).

% /
sumDiagonalRL(_,Max,Step) when Step < 1 -> 0;
sumDiagonalRL(Matrix, Max, Step)-> getCell(Matrix, Max - Step + 1, Step, Max) + sumDiagonalRL(Matrix, Max, Step - 1).

checkMatrix(Matrix, N)-> [lists:sum(getRow(Matrix, Row, N)) || Row <- lists:seq(1,N)].




getRow(Matrix, R, N)-> [getCell(Matrix,X,Y,N) || X <- lists:seq(1,N), Y <- [R]].
getCol(Matrix, C, N)-> [getCell(Matrix,X,Y,N) || X <- [C], Y <- lists:seq(1,N)].

getCell(Matrix, X, Y, N) -> lists:nth(N*(Y-1) + X, Matrix).

getLastRow(Part, N, MagicNumber)-> [MagicNumber - Sum || Col <- lists:seq(1,N), Sum <- [colSum(Part, Col, 0, N)], lists:member(MagicNumber - Sum, lists:seq(1,N*N)) ].

colSum(Part, Col, Row, N) when Row == N-1 -> 0;
colSum(Part, Col, Row, N)-> lists:nth(Row*N+Col,Part) + colSum(Part, Col, Row+1, N).



% combineSquares ermittelt aus allen Teilquadraten die gueltige Loesung
% Aufruf: combineSquares(Parts, Max, Value)
% Parts - Alle Teilquadrate
% Max - Anzahl der Zeilen
% Value - Wert der Summe einer Zeile
-spec combineSquares(list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(), integer())->list(list((non_neg_integer()))).
combineSquares([],_, _, _) -> [];
combineSquares([X|XS], Max, Value, Num) ->
	Res= calcSquares(X,Max,Value),
	case Res of
		[] -> combineSquares(XS, Max, Value, Num);
		_ ->	io:format("Erg Nummer~p:~p~n",[Num,Res]),Res++combineSquares(XS, Max, Value,Num+length(Res))
	end.

combineSquares(Parts, Max, Value) ->
	lists:flatmap(fun(X)->calcSquares(X,Max,Value) end, Parts).


magicsquare(Max)-> magicsquare(Max, egal).
magicsquare(Max, Mode)->
	statistics(runtime),
	Result= case Mode of
			debug ->  case Max of
					3-> Parts= combineRows(2,3,15), combineSquares(Parts,3,15,0);
					4-> Parts= combineRows(1,4,34), combineSquares(Parts,4,34,0);
					_-> error
				end;
			_ -> case Max of
					3-> Parts= combineRows(2,3,15), combineSquares(Parts,3,15);
					4-> Parts= combineRows(2,4,34), combineSquares(Parts,4,34);
					_-> error
				end
	end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%												%	%												%
% 		    Hier beginnt die Verteilung des Algorithmus					%				%												%				%												%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%   Verteilung auf einem Rechner   %%%%%%%%%%%%%%%%%%

% Berechnung Magischer Quadrate
% Funktioniert fuer N=3 und N=4
% Aufruf: distribMS(Max, PCount)
% Max - Anzahl der Reihen/Spalten
% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
% oder wenn X=4, dann ist die Summe gleich 34
-spec distribMS(non_neg_integer(), non_neg_integer())-> list(list(non_neg_integer())).
distribMS(Max, PCount)->
	statistics(runtime),
	Result=
		case Max of
			3 -> Value=15, PSquare=combineRows(1,Max,Value),
				spawn_at(PCount, node(), PSquare, 3, Value, init_local),
				loop_gather(PCount,[]);
			4 -> Value=34, PSquare=combineRows(2,Max,Value),
				spawn_at(PCount, node(), PSquare, 4, Value, init_local),
				loop_gather(PCount,[]);
			_ ->  [[]]
		end,
	{_, Time1} = statistics(runtime),
	U = Time1 / 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	Result.


autoDistrib(Max)->distribMS(Max, erlang:system_info(logical_processors_available)).


t()->
S1 = [[3,5,7],[2,6,7],[6,1,8],[5,2,8],[4,3,8],[3,4,8],[2,5,8],[1,6,8],[5,1,9],[4,2,9],[2,4,9],[1,5,9]],
S2 = [[5,7,3],[4,8,3],[9,2,4],[8,3,4],[6,5,4],[5,6,4],[3,8,4],[2,9,4],[9,1,5],[8,2,5],[7,3,5],[6,4,5]],
S3 = [[9,5,1],[8,6,1],[6,8,1],[5,9,1],[9,4,2],[8,5,2],[7,6,2],[6,7,2],[5,8,2],[4,9,2],[8,4,3],[7,5,3]],
S4 = [[4,6,5],[3,7,5],[2,8,5],[1,9,5],[8,1,6],[7,2,6],[5,4,6],[4,5,6],[2,7,6],[1,8,6],[6,2,7],[5,3,7]],
[
duplicate(S1,S2),
duplicate(S1,S3),
duplicate(S1,S4),
duplicate(S2,S3),
duplicate(S2,S4),
duplicate(S3,S4)
].




% Spawnt eine festgelegte Anzahl von Prozessen auf einem angegebenen Host
% Aufruf: spawn_at(CCount, Host, Count, Plist, Max, Value)
% CCount - Anzahl der Prozesse, die abgespalten werden sollen
% Host - Host auf dem der Prozess erzeugt werden soll / wird in diesem Teil nicht benoetigt,
% 		 da alles auf dem lokalen Rechner stattfindet
% InitFun - Funktion, die beim Initialisieren des Prozesses aufgerufen werden soll
%-spec spawn_at(integer(), atom(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(), atom()).

spawn_at(CCount, Host, PList, Max, Value, InitFun)->  spawnParam(CCount, Host, PList, Max, Value, InitFun, 0).


spawnParam(CCount, Host, PList, Max, Value, InitFun, SpawnCounter) when SpawnCounter == CCount ->done;
spawnParam(CCount, Host, PList, Max, Value, InitFun, SpawnCounter)->
													Len = trunc(length(PList)/CCount),
													Rest = case CCount - SpawnCounter  of 
																				1 -> SpawnCounter;
																				_ -> 0 end,
													Args = [CCount, self(), lists:sublist(PList, Len * SpawnCounter + 1, Len + Rest), Max, Value, Host],
													Pid = spawn(Host, ?MODULE, InitFun, Args),
													
													%spawn(Host, ?MODULE, fun()->print(dude_g) end),
													
													spawnParam(CCount, Host, PList, Max, Value, InitFun, SpawnCounter+1).



% Methode, die bei Abspaltung des Prozesses aufgerufen wird
% hat die/den Parameter [Nr, SPid, PList, Max, Value, Host]
% Die Methode berechnet fuer eine Menge an Teilquadraten alle Loesun!gen und
% sendet diese an den erzeugenden Prozess.
% Nr - Nummer des Prozesses (nur fuer debug-Ausgaben auf der Konsole)
% SPid - Prozessnummer des erzeugenden Prozesses - fuer das Senden des Ergebnisses
% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
% Max - Anzahl der Spalten/Zeilen
% Value - Wert der Summe der Zeile
% Host - kann hier vernachlaessigt werden

init_local(Nr, SPid, PList, Max, Value, _)-> print(dude, init_local),
	distrib_calc_squares(Nr, SPid, PList, Max, Value).

-spec distrib_calc_squares(non_neg_integer(), pid(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer()) -> ok.
%distrib_calc_squares(Nr, SPid, PList, Max, Value)-> Res = combineSquares(PList,Max,Value), SPid!Res.
distrib_calc_squares(Nr, SPid, PList, Max, Value)-> 

													Res = combineSquares(PList,Max,Value),
													SPid!{calc, Res}.

% Methode sammelt alle Ergebnisse ein
% Wird von der Methode magicsquare aufgerufen
% Aufruf (CCount, Result)
% CCount - Anzahl der Prozesse, die gestartet wurden (entspricht der Anzahl der
%		   zu erwartenden Ergebnisse
% Result - Aktuell bereitstehendes Ergebnis

%-spec loop_gather(non_neg_integer(), list(list(non_neg_integer())))-> list(list(non_neg_integer())).
loop_gather(0,Result)-> Result;		
loop_gather(CCount,Result)->
							receive
								{calc, Res} ->loop_gather(CCount-1,Result ++ Res) 
							%after 3000 -> timeout
							end.

											

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%								   									%%
%%		Verteilung auf mehrere Rechner			   					%%
%%								   									%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Codieren der Hostnamen mit der Anzahl von Prozessen, die sie ausfuehren sollen
%hosts()->[{'tiger@hadoop03',48},{'scorpion@hadoop06',48}	3].
hosts()->[{'pad@thinkpad',2},{'think@thinkpad',2}].


% Berechnung der Anzahl der Prozesse insgesamt
% Soll fuer die Aufteilung der Quadrate verwendet werden
c_count()-> lists:sum([Count||{_,Count}<-hosts()]).


% Berechnung Magischer Quadrate
% Funktioniert fuer N=3 und N=4
% Aufruf: distribMS(Max, PCount)
% Max - Anzahl der Reihen/Spalten
% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
% oder wenn X=4, dann ist die Summe gleich 34

megaDistribMS(Max)->

	% Ausschalten des Error-Loggings auf der Konsole
	error_logger:tty(true),
	register(host_monitor,spawn(fun()->init_host_monitor(hosts()) end)),
	statistics(runtime),
	Result=
		case Max of
			3 -> Value=15, PSquare=combineRows(2,Max,Value),
				while(c_count(), hosts(), PSquare, 3, 15),
%				spawn_at(4, node(), PSquare, 3, Value),
				loop_gather(c_count(),[]);
			4 -> Value=34, PSquare=combineRows(2,Max,Value),
				while(c_count(), hosts(), PSquare, 4, 34),
%				spawn_at(4, node(), PSquare, 4, Value),
				loop_gather(c_count(),[]);
			_ ->  [[]]
		end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	print(Result),
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	host_monitor!stop,
	Result.

% Schleife fuer das spawnen der Prozesse auf mehreren Rechnern
% Benutzt die Methode spawn_at(...)
% Aufruf: while (CCount, Hosts, PList, Max, Value)
% CCount - Anzahl der Prozesse die gespawnt werden sollen
% Hosts - Hostliste der Form { VM-Name, Anzahl der Prozesse}
% PList - Liste der Teilquadrate
% Max - Anzahl der Elemente, die berechnet werden sollen
% Value - Wert der Summe der Zeile
-spec while(non_neg_integer(), list({atom(),non_neg_integer()}), list(list(non_neg_integer())), non_neg_integer(),non_neg_integer())->ok.
while (CCount, Hosts, PList, Max, Value) -> whileParam(CCount, Hosts, PList, Max, Value, 0, trunc(length(PList)/length(hosts()))).


whileParam(CCount, [{ VMName, VMPCount}], PList, Max, Value, HostCounter,Len)-> spawn_at(VMPCount, VMName, lists:nthtail(Len * HostCounter,PList), Max, Value, init_global);
whileParam(CCount, [{ VMName, VMPCount} | Hosts], PList, Max, Value, HostCounter, Len)->
																	Hcount = length(hosts()), 
																	Rest = case length(Hosts) of 
																				0 -> Hcount-1;
																				_ -> 0 end,
																	PListDist = lists:sublist(PList, Len * HostCounter + 1, Len + Rest),

																	spawn_at(VMPCount, VMName, lists:sublist(PList, Len * HostCounter + 1, Len + Rest), Max, Value, init_global), 
																	whileParam(CCount, Hosts, PList, Max, Value, HostCounter + 1, Len).




% Supervisor-Prozess, der die Ausfuehrung der Berechnungen ueberwacht
% Spawnt die Berechnungsprozesse auf den Nodes des Erlang-Clusters und behandelt die Fehlerfaelle
% Nr - Nummer des Prozesses (nur zur besseren Identifikation)
% SPid - Prozessnummer des erzeugenden Prozesses
% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
% Max - Anzahl der Spalten/Zeilen
% Value - Wert der Summe der Zeile
% Try - Anzahl der noch ausstehenden Versuche

init_global(Nr, SPid, PList, Max, Value, Host)-> print(dude, init_global),
	init_global(Nr, SPid, PList, Max, Value, Host,3).


-spec init_global(non_neg_integer(), pid(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(),
	atom(), non_neg_integer()) -> ok.
init_global(Nr, SPid, PList, Max, Value, Host, Try)->
				{_,PCount} = lists:keyfind(node(), 1, hosts()),
				spawn_at(PCount, node(), PList, Max, Value, init_local),
				Result =loop_gather(PCount,[]), 
				SPid!{calc, Result}.





% Monitoring-Prozess fuer die Ueberwachung der zur Verfuegung stehenden Cluster-Nodes
% Er wird von der Hauptmethode megaDistribMS gestartet,
% Der Prozess kann ueber das Atom host_monitor angesprochen werden.
% Er beinhaltet die folgenden Operationen:
%  getnode - Ermittlung eines verfuegbaren Nodes
%  addnode - Hinzunahme eines Nodes
%  gethosts - Ermittlung aller verfuegbaren Hosts
%  deletenode - Loeschen eines Nodes

init_host_monitor(MonitorList) -> ML= lists:map(fun({Host,_})->Host end, MonitorList),
	lists:foreach(fun(Host)->erlang:monitor_node(Host, true) end, ML),
	monitorHosts(ML).

monitorHosts([])-> erlang:error(no_hosts_available);
monitorHosts(HostList)->
	receive
		{nodedown, NodeName}-> io:format("Host ~p is down!~n",[NodeName]),
			monitorHosts(lists:delete(NodeName, HostList));
		{getnode, From}-> io:format("Host ~p is requested!~n",[hd(HostList)]),
			From!{newhost, hd(HostList)}, monitorHosts(tl(HostList)++[hd(HostList)]);
		{addnode, NodeName}-> io:format("Host ~p is added!~n",[NodeName]),
			monitor_node(NodeName, true),
			monitorHosts([NodeName|HostList]);
		{gethosts, From} -> From!{hostlist, HostList}, monitorHosts(HostList);
		{deletenode, NodeName}-> io:format("Host ~p will be deleted!~n",[NodeName]),
			monitorHosts(lists:delete(NodeName, HostList));
		stop -> ok
	end.
