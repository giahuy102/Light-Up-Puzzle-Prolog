%Check if the cell is bulb or not. If bulb -> increase CountCell to 1
countCell([X, Y], Bulbs, CountCell) :-
    member([X, Y], Bulbs),
    !, 
    CountCell is 1.
countCell([X, Y], Bulbs, CountCell) :-
    CountCell is 0.


%Count the number bulb cells around cell [X, Y]
countHaveCells([X, Y], Bulbs, CountHaveCells) :- 
    TempX1 is X - 1,
    countCell([TempX1, Y], Bulbs, CountUpCell),
    TempX2 is X + 1,
    countCell([TempX2, Y], Bulbs, CountDownCell),
    TempY1 is Y - 1,
    countCell([X, TempY1], Bulbs, CountLeftCell),
    TempY2 is Y + 1,
    countCell([X, TempY2], Bulbs, CountRightCell),
    CountHaveCells is CountLeftCell + CountRightCell + CountUpCell + CountDownCell.


%Check if can put a bulb around black cell
freeBlackCell([X, Y, Z], Bulbs) :-
    countHaveCells([X, Y], Bulbs, CountHaveCells),
    CountRemainCells is Z - CountHaveCells,
    (
        Z == 5
    ->  CountRemainCells >= 2;
        CountRemainCells >= 1
    ).


%Check if a black cell satisfy constraint of problem (HaveCells = NeedCells)
fullBlackCell([X, Y, Z], Bulbs) :-
    countHaveCells([X, Y], Bulbs, CountHaveCells),
    CountRemainCells is Z - CountHaveCells,
    (
        Z \== 5
    ->  CountRemainCells == 0;
        CountRemainCells =< 5
    ).


%Check if all black cells around a white cell is free
satisfyBlackCellsConstraint([X, Y], [], Bulbs).
satisfyBlackCellsConstraint([X, Y], [[X1, Y1, Z1] | Tail], Bulbs) :-
    TempX1 is X - 1,
    TempX2 is X + 1,
    TempY1 is Y - 1,
    TempY2 is Y + 1,
    (
        [TempX1, Y] = [X1, Y1];
        [TempX2, Y] = [X1, Y1];
        [X, TempY1] = [X1, Y1];
        [X, TempY2] = [X1, Y1]
    ),
    !,
    freeBlackCell([X1, Y1, Z1], Bulbs),
    satisfyBlackCellsConstraint([X, Y], Tail, Bulbs),
    !.
satisfyBlackCellsConstraint([X, Y], [[X1, Y1, Z1] | Tail], Bulbs).


%check if a white cell can be put a bulb
satisfyCellConstraint(Cell, WhiteCells, BlackCells, Bulbs, Width, Height, LightCells) :-
    \+ member(Cell, LightCells),
    satisfyBlackCellsConstraint(Cell, BlackCells, Bulbs).


%check all white cells in the left of current cells as light cells (by appending to list)
appendLeft([_, -1], LeftCells, WhiteCells, CurrentLightCells) :-
    !, 
    LeftCells = [].
appendLeft(Cell, LeftCells, WhiteCells, CurrentLightCells) :- 
    \+ member(Cell, WhiteCells),
    !,
    LeftCells = [].
appendLeft([X, Y], LeftCells, WhiteCells, CurrentLightCells) :-
    Temp is Y - 1,
    appendLeft([X, Temp], TempCells, WhiteCells, CurrentLightCells),
    (
        \+ member([X, Y], CurrentLightCells)
    ->  append([[X, Y]], TempCells, LeftCells);
        LeftCells = TempCells
    ).
    

%check all white cells in the right of current cells as light cells (by appending to list)
appendRight([_, Width], RightCells, WhiteCells, Width, CurrentLightCells) :-
    !, 
    RightCells = [].
appendRight(Cell, RightCells, WhiteCells, Width, CurrentLightCells) :- 
    \+ member(Cell, WhiteCells),
    !,
    RightCells = [].
appendRight([X, Y], RightCells, WhiteCells, Width, CurrentLightCells) :-
    Temp is Y + 1,
    appendRight([X, Temp], TempCells, WhiteCells, Width, CurrentLightCells),
    (
        \+ member([X, Y], CurrentLightCells)
    ->  append([[X, Y]], TempCells, RightCells);
        RightCells = TempCells
    ).


%check all white cells in the up of current cells as light cells (by appending to list)
appendUp([-1, _], UpCells, WhiteCells, CurrentLightCells) :-
    !, 
    UpCells = [].
appendUp(Cell, UpCells, WhiteCells, CurrentLightCells) :- 
    \+ member(Cell, WhiteCells),
    !,
    UpCells = [].
appendUp([X, Y], UpCells, WhiteCells, CurrentLightCells) :-
    Temp is X - 1,
    appendUp([Temp, Y], TempCells, WhiteCells, CurrentLightCells),
    (
        \+ member([X, Y], CurrentLightCells)
    ->  append([[X, Y]], TempCells, UpCells);
        UpCells = TempCells
    ).


%check all white cells in the down of current cells as light cells (by appending to list)
appendDown([Height, _], DownCells, WhiteCells, Height, CurrentLightCells) :-
    !, 
    DownCells = [].
appendDown(Cell, DownCells, WhiteCells, Height, CurrentLightCells) :- 
    \+ member(Cell, WhiteCells),
    !,
    DownCells = [].
appendDown([X, Y], DownCells, WhiteCells, Height, CurrentLightCells) :-
    Temp is X + 1,
    appendDown([Temp, Y], TempCells, WhiteCells, Height, CurrentLightCells),
    (
        \+ member([X, Y], CurrentLightCells)
    ->  append([[X, Y]], TempCells, DownCells);
        DownCells = TempCells
    ).


%check all white cells to the 4 directions as light cells (by appending to list)
appendLightCells([X, Y], LightCells, WhiteCells, Width, Height, CurrentLightCells) :-
    TempY1 is Y - 1,
    TempY2 is Y + 1,
    TempX1 is X - 1, 
    TempX2 is X + 1,
    appendLeft([X, TempY1], LeftCells, WhiteCells, CurrentLightCells),
    appendRight([X, TempY2], RightCells, WhiteCells, Width, CurrentLightCells),
    appendUp([TempX1, Y], UpCells, WhiteCells, CurrentLightCells),
    appendDown([TempX2, Y], DownCells, WhiteCells, Height, CurrentLightCells),
    append(LeftCells, [], Temp1),
    append(RightCells, Temp1, Temp2),
    append(UpCells, Temp2, Temp3),
    append(DownCells, Temp3, LightCells).


%find length of a list
list_length([],0).
list_length([_|TAIL], N) :- list_length(TAIL, N1), N is N1 + 1.


%check if all black cells satisfy problem constraint
satisfyAllBlackCells([], Bulbs).
satisfyAllBlackCells([[X, Y, Z] | Tail], Bulbs) :-
    fullBlackCell([X, Y, Z], Bulbs),
    satisfyAllBlackCells(Tail, Bulbs).


%End recursive of problem constraints is satisfied
solve(Cells, WhiteCells, BlackCells, Width, Height, Bulbs, TempBulbs, LightCells) :-
    list_length(WhiteCells, N),
    list_length(TempBulbs, N1),
    list_length(LightCells, N2),
    N is N1 + N2,
    satisfyAllBlackCells(BlackCells, TempBulbs),
    Bulbs = TempBulbs,
    !.


%recursive predicate to solve the problem
solve([Head | Tail], WhiteCells, BlackCells, Width, Height, Bulbs, TempBulbs, LightCells) :-
    satisfyCellConstraint(Head, WhiteCells, BlackCells, TempBulbs, Width, Height, LightCells),
    append([Head], TempBulbs, NewBulbs),
    appendLightCells(Head, TempLightCells, WhiteCells, Width, Height, LightCells),
    append(TempLightCells, LightCells, NewLightCells),
    % write(NewLightCells), nl,
    % write(NewBulbs), nl,
    % write(Tail), nl, nl,
    solve(Tail, WhiteCells, BlackCells, Width, Height, Bulbs, NewBulbs, NewLightCells),
    !.
solve([Head | Tail], WhiteCells, BlackCells, Width, Height, Bulbs, TempBulbs, LightCells) :-
    solve(Tail, WhiteCells, BlackCells, Width, Height, Bulbs, TempBulbs, LightCells).


%Call solver
lightup(WhiteCells, BlackCells, Width, Height, Bulbs) :-
    solve(WhiteCells, WhiteCells, BlackCells, Width, Height, Bulbs, [], []).


% Pretty printer to print the coordinates of each bulb in a row.
printBulbs([]).
printBulbs([[R, C] | Bulbs]) :-
	write(R), write(' '), write(C), nl,
	printBulbs(Bulbs).









    


