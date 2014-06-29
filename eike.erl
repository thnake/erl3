-module(bel3_1).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	
%	Algorithmus fuer die verteilte Berechnung Magischer Quadrate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Teil 1 - Berechnung magischer Quadrate auf einem Rechner  %%%%

% Berechnet alle moeglichen Zeilen eines Magischen Quadrats
% Aufruf: row(Max, Value, Elements) - z.B. row(3, 15, lists:seq(1, 9))
% Max - Seitengroesse des Quadrats
% Value - Wert der Summe der Zeile
% Elements - Elemente aus denen ausgewaehlt werden soll
-spec row(non_neg_integer(), non_neg_integer(), list(non_neg_integer())) -> list(list(non_neg_integer())).
row(Max, Value, Elements) ->

  Lists = generateLists(Max, Elements),
  ListWithProperSum = removeWrongSum(Value, cartesian(Lists)),
  ListWouthoutDublicates = removeDuplicates(ListWithProperSum, []),
  ListWouthoutDublicates. %Eventuell Ergebnismenge spiegeln

generateLists(Max, Elements) -> [Elements || A <- lists:seq(1, Max)].

removeWrongSum(Sum, List) -> [A || A <- List, sum(A) == Sum].
sum(List) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, List).

cartesian([H | T]) -> [[A | B] || A <- H, B <- cartesian(T)];
cartesian([]) -> [[]].

removeDuplicates([], Acc) -> Acc;
removeDuplicates([H | T], Acc) -> case hasDuplicates(H) of
                                    true -> removeDuplicates(T, Acc);
                                    _ -> removeDuplicates(T, [H | Acc])
                                  end.


hasDuplicates(List) -> erlang:length(List) /= sets:size(sets:from_list(List)).


%splitList(Max, Elements) -> [lists:sublist(Elements, Max * A - Max - 1) || A <- lists:seq(1, Max)].

% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
% Aufruf duplicate(Liste1,Liste2)
% Liste1 - Erste Liste
% Liste2 - Zweite Liste
-spec duplicate(list(non_neg_integer()), list(non_neg_integer())) -> true | false.
duplicate([], _) -> false;
duplicate(_, []) -> false;
duplicate(List1, List2) ->
  case
  lists:member(hd(List1), List2) of true ->
    true;
    _ ->
      duplicate(tl(List1), List2)
  end.

% combineRows setzt eine beliebige Anzahl von Reihen, die vorab berechnet werden, zusammen
% Dabei wird ueberprueft, ob sich doppelte Elemente innerhalb der Reihen befinden.
% Aufruf: combineRows (Col, Max, Value)
% Col - Anzahl der Reihen, die berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile
% Value - Wert der Summe der Zeile
% Elems - Elemente aus denen gewaehlt werden soll

-spec combineRows(non_neg_integer(), non_neg_integer(), non_neg_integer(), list(non_neg_integer())) -> list(list(non_neg_integer())).
combineRows(Col, Max, Value, Elems) ->
  Rows = row(Max, Value, Elems),
  cartesianLists(Rows).


cartesianLists(List) -> removeDuplicates([lists:flatten([A | B]) || A <- List, B <- List], []);
cartesianLists([]) -> [[]].


%[[1,2,3,4,5,6],[7,8,9,10,11,12]]

% calcSquares berechnet aus einem Teilquadrat alle moeglichen gueltigen Quadrate, die sich bilden lassen
% Aufruf: calcSquares(Part, Max, Value)
% Part - Teilquadrat fuer das die Magischen Quadrate berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile/Spalte
% Value - Wert der Summe einer Zeile
-spec calcSquares(list(non_neg_integer()), non_neg_integer(), non_neg_integer()) -> list(list(non_neg_integer())).
calcSquares(Part, Max, Value) ->
  Remain = Max - (length(Part) / Max),
  case Remain of
    0 -> Part;
    _ -> calcSquares(combineRows(1, Max, Value, Part), Max, Value)
  end.


% combineSquares ermittelt aus allen Teilquadraten die gueltige Loesung
% Aufruf: combineSquares(Parts, Max, Value)
% Parts - Alle Teilquadrate
% Max - Anzahl der Zeilen
% Value - Wert der Summe einer Zeile
-spec combineSquares(list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(), integer()) -> list(list((non_neg_integer()))).
combineSquares([], _, _, _) -> [];
combineSquares([X | XS], Max, Value, Num) ->
  Res = calcSquares(X, Max, Value),
  case Res of
    [] -> combineSquares(XS, Max, Value, Num);
    _ -> io:format("Erg Nummer~p:~p~n", [Num, Res]), Res ++ combineSquares(XS, Max, Value, Num + length(Res))
  end.

combineSquares(Parts, Max, Value) ->
  lists:flatmap(fun(X) -> calcSquares(X, Max, Value) end, Parts).


magicsquare(Max) -> magicsquare(Max, egal).
magicsquare(Max, Mode) ->
  statistics(runtime),
  Result = case Mode of
             debug -> case Max of
                        3 -> Parts = combineRows(2, 3, 15, lists:seq(1, 9)), combineSquares(Parts, 3, 15, 0);
                        4 -> Parts = combineRows(1, 4, 34, lists:seq(1, 16)), combineSquares(Parts, 4, 34, 0);
                        _ -> error
                      end;
             _ -> case Max of
                    3 -> Parts = combineRows(2, 3, 15, lists:seq(1, 9)), combineSquares(Parts, 3, 15);
                    4 -> Parts = combineRows(2, 4, 34, lists:seq(1, 16)), combineSquares(Parts, 4, 34);
                    _ -> error
                  end
           end,
  {_, Time1} = statistics(runtime),
  U = Time1 / 1000,
  io:format("Anzahl der Quadrate:~p~n", [length(Result)]),
  io:format("Magicsquare Time:~p~n", [U]),
  Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%																									%
% 	Hier beginnt die Verteilung des Algorithmus														%
%																									%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%   Teil 2 - Verteilung auf einem Rechner   %%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%% Teil 3 - Verteilung auf mehreren Rechnern %%%%%%%%%%%%%%%%%%
