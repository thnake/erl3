-module(bel3).
-compile(export_all).

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
row(Max, Value, Elements) ->  [lists:sort(Y) || Y <- comb(Elements, Max), lists:sum(Y) == Value, hasDoubles(Y)].

comb(_,0) -> [ [] ];
comb(Elements, Count) -> [ Y++[Q] || Y<-comb(Elements,Count-1), Q<-Elements].

hasDoubles(List)-> length(sets:to_list(sets:from_list(List))) == length(List).

t()->row(3,15, lists:seq(1,9)).

tc()->R = t(), check(15, R). 


check(Sum,[]) ->true;
check(Sum, [H|T])  ->case lists:sum(H) of
                    Sum -> check(Sum, T);
                    _ -> false end.


checkl(L,[]) ->true;
checkl(L, [H|T])  ->case length(H) of
                    L -> checkl(L, T);
                    _ -> H end.

% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
% Aufruf duplicate(Liste1,Liste2)
% Liste1 - Erste Liste
% Liste2 - Zweite Liste
-spec duplicate(list(non_neg_integer()),list(non_neg_integer())) -> true | false.
duplicate(Liste1,Liste2 )-> toBeDefined.

% combineRows setzt eine beliebige Anzahl von Reihen, die vorab berechnet werden, zusammen
% Dabei wird ueberprueft, ob sich doppelte Elemente innerhalb der Reihen befinden.
% Aufruf: combineRows (Col, Max, Value)
% Col - Anzahl der Reihen, die berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile
% Value - Wert der Summe der Zeile
% Elems - Elemente aus denen gewaehlt werden soll

-spec combineRows(non_neg_integer(), non_neg_integer(), non_neg_integer(), list(non_neg_integer()))->list(list(non_neg_integer())).
combineRows(Col,Max,Value, Elems) -> toBeDefined. 
combineRows(Col,Max,Value) -> dummy.
% calcSquares berechnet aus einem Teilquadrat alle moeglichen gueltigen Quadrate, die sich bilden lassen
% Aufruf: calcSquares(Part, Max, Value)
% Part - Teilquadrat fuer das die Magischen Quadrate berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile/Spalte
% Value - Wert der Summe einer Zeile
-spec calcSquares(list(non_neg_integer()), non_neg_integer(), non_neg_integer()) -> list(list(non_neg_integer())).
calcSquares(Part, Max, Value)-> toBeDefined. 

	
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
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	Result.

% Spawnt eine festgelegte Anzahl von Prozessen auf einem angegebenen Host
% Aufruf: spawn_at(CCount, Host, Count, Plist, Max, Value)
% CCount - Anzahl der Prozesse, die abgespalten werden sollen
% Host - Host auf dem der Prozess erzeugt werden soll / wird in diesem Teil nicht benoetigt,
% 		 da alles auf dem lokalen Rechner stattfindet
% InitFun - Funktion, die beim Initialisieren des Prozesses aufgerufen werden soll
-spec spawn_at(integer(), atom(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(), atom()) -> ok.
spawn_at(CCount, Host, PList, Max, Value, InitFun)-> toBeDefined.

% Methode, die bei Abspaltung des Prozesses aufgerufen wird
% hat die/den Parameter [Nr, SPid, PList, Max, Value, Host]
% Die Methode berechnet fuer eine Menge an Teilquadraten alle Loesungen und
% sendet diese an den erzeugenden Prozess.
% Nr - Nummer des Prozesses (nur fuer debug-Ausgaben auf der Konsole)
% SPid - Prozessnummer des erzeugenden Prozesses - fuer das Senden des Ergebnisses
% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
% Max - Anzahl der Spalten/Zeilen
% Value - Wert der Summe der Zeile 
% Host - kann hier vernachlaessigt werden 
init_local(Nr, SPid, PList, Max, Value,_)-> 
	distrib_calc_squares(Nr, SPid, PList, Max, Value).

-spec distrib_calc_squares(non_neg_integer(), pid(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer()) -> ok.
distrib_calc_squares(Nr, SPid, PList, Max, Value)-> toBeDefined.

% Methode sammelt alle Ergebnisse ein
% Wird von der Methode magicsquare aufgerufen
% Aufruf (CCount, Result)
% CCount - Anzahl der Prozesse, die gestartet wurden (entspricht der Anzahl der
%		   zu erwartenden Ergebnisse
% Result - Aktuell bereitstehendes Ergebnis

-spec loop_gather(non_neg_integer(), list(list(non_neg_integer())))-> list(list(non_neg_integer())).
loop_gather(CCount,Result)-> toBeDefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%								   									%%
%%		Verteilung auf mehrere Rechner			   					%%
%%								   									%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Codieren der Hostnamen mit der Anzahl von Prozessen, die sie ausfuehren sollen
hosts()->[{'tiger@hadoop03',48},{'scorpion@hadoop06',48}].

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
	error_logger:tty(false),
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
while (CCount, HostCountL, PList, Max, Value) -> toBeDefined.
	
% Supervisor-Prozess, der die Ausfuehrung der Berechnungen ueberwacht
% Spawnt die Berechnungsprozesse auf den Nodes des Erlang-Clusters und behandelt die Fehlerfaelle
% Nr - Nummer des Prozesses (nur zur besseren Identifikation)
% SPid - Prozessnummer des erzeugenden Prozesses
% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
% Max - Anzahl der Spalten/Zeilen
% Value - Wert der Summe der Zeile 
% Try - Anzahl der noch ausstehenden Versuche

init_global(Nr, SPid, PList, Max, Value, Host)->
	init_global(Nr, SPid, PList, Max, Value, Host,3).

-spec init_global(non_neg_integer(), pid(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(),
	atom(), non_neg_integer()) -> ok.	
init_global(Nr, SPid, PList, Max, Value, Host, Try)-> toBeDefined.

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
