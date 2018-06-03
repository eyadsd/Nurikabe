
printTable:-tableSize(M,N), printTable(1,1,M,N).

printTable(X,Y,N,M):-
    Y<M,X=<N,
    (cell(X,Y,Z);fxd_cell(X,Y,Z)),
    write(Z),
    Y1 is Y + 1,
    printTable(X,Y1,N,M).

printTable(X,Y,N,Y):-
    X=<N,
    (cell(X,Y,Z);fxd_cell(X,Y,Z)),
    write(Z),
    nl,
    X1 is X + 1,
    printTable(X1,1,N,Y).

fixed(X,Y,Z):-
    retract(cell(X,Y,_)),
    assert(fxd_cell(X,Y,Z)).

makeTable(N,M):-retractall(cell(_,_,_)),
    retractall(fxd_cell(_,_,_)),
    retractall(tableSize(_,_)),
    assert(tableSize(N,M)),
    makeTable(1,1,N,M).

makeTable(X,Y,N,M):-
    X=<N,Y<M,
    assert(cell(X,Y,g)),
    Y1 is Y + 1,
    makeTable(X,Y1,N,M).

makeTable(X,Y,N,Y):-
    X=<N,
    assert(cell(X,Y,g)),
    X1 is X + 1,
    makeTable(X1,1,N,Y).


solve(X,Y,Z):- not(fxd_cell(X,Y,_)),
    retractall(cell(X,Y,_)),
    assert(cell(X,Y,Z)),
    retractall(islandsCountOK),
    assert(islandsCountOK),
    retractall(islandsNotConnected),
    assert(islandsNotConnected),
    printTable.

block(X,Y):- X1 is X+1, Y1 is Y + 1, cell(X,Y,b), cell(X1,Y,b),cell(X,Y1,b),cell(X1,Y1,b).


noBlocks:-tableSize(M,N), noBlocks(1,1,M,N).

noBlocks(X,Y,M,N) :-
    Y<M,X=<N,
    not(block(X,Y)),
    Y1 is Y + 1,
    noBlocks(X,Y1,N,M).

noBlocks(X,Y,N,Y):-
    X=<N,
    not(block(X,Y)),
    X1 is X + 1,
    noBlocks(X1,1,N,Y).

noBlocks(M,N,M,N):- write(noBlocks),nl.


findBlueCell(X,Y):- cell(X,Y,b),!.

traverseBlueCells :-retractall(visited(_,_)),
                   findBlueCell(X,Y),
                   traverseBlueCells(X,Y).

traverseBlueCells(X,Y):-  X1 is X+1,X2 is X-1,
                          Y1 is Y+1,Y2 is Y-1,
                          assert(visited(X,Y)),
                          (cell(X1,Y,b),not(visited(X1,Y)),traverseBlueCells(X1,Y);
                          cell(X,Y1,b),not(visited(X,Y1)),traverseBlueCells(X,Y1);
                          cell(X,Y2,b),not(visited(X,Y2)),traverseBlueCells(X,Y2);
                          cell(X2,Y,b),not(visited(X2,Y)),traverseBlueCells(X2,Y)).

checkBlueCells:-tableSize(M,N), checkBlueCells(1,1,M,N).

checkBlueCells(X,Y,M,N) :-
      Y<M,X=<N,
     (visited(X,Y);not(cell(X,Y,b))),
      Y1 is Y + 1,
      checkBlueCells(X,Y1,N,M).

checkBlueCells(X,Y,N,Y):-
      X=<N,
      (visited(X,Y);not(cell(X,Y,b))),
      X1 is X + 1,
      checkBlueCells(X1,1,N,Y).

checkBlueCells(X,Y,X,Y):- visited(X,Y);not(cell(X,Y,b)).


blueCellsConnected:- traverseBlueCells;
                     checkBlueCells,
                     write('blue cells are connected').

blueCellsOK:- noBlocks,blueCellsConnected.

findIslands:- retractall(island(_,_,_)),
              fxd_cell(X,Y,Z),
              retractall(visited(_,_)),
              findIslands(X,Y,Z).

findIslands(X,Y,Z):- island(X,Y,_),retractall(islandsNotConnected),assert(islandsNotConnected:- fail),fail;
                     X1 is X+1,X2 is X-1,
                     Y1 is Y+1,Y2 is Y-1,
                     assert(island(X,Y,Z)),assert(visited(X,Y)),(
                     cell(X1,Y,g),not(visited(X1,Y)),findIslands(X1,Y,Z);
                     cell(X,Y1,g),not(visited(X,Y1)),findIslands(X,Y1,Z);
                     cell(X,Y2,g),not(visited(X,Y2)),findIslands(X,Y2,Z);
                     cell(X2,Y,g),not(visited(X2,Y)),findIslands(X2,Y,Z)).

countIsland:-island(_,_,Z),
    aggregate_all(count,island(_,_,Z),Count),
    write(Z),nl,
    Count \= Z,
    retractall(islandsCountOK),
    assert(islandsCountOK:- fail),fail.

solveIslands:- findIslands;countIsland.

islandsOK:- solveIslands;islandsNotConnected,islandsCountOK.


checkSolution:- blueCellsOK,islandsOK.


try :-cell(X,Y,_),(solve(X,Y,g);solve(X,Y,b)),checkSolution,fail.
