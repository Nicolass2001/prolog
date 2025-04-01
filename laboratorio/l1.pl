palabra([a,l]).
palabra([l,o]).
palabra([a,s]).
palabra([s,i]).
palabra([l,a]).
palabra([l,e]).
palabra([a,s]).
palabra([s,e]).
palabra([a,l,a]).
palabra([c,a,n]).
palabra([a,s,a]).
palabra([a,c,a]).
palabra([a,m,a]).
palabra([c,a,l]).
palabra([m,a,l]).
palabra([m,a,s]).
palabra([l,a,s]).
palabra([a,n,a]).
palabra([m,e,s,a]).
palabra([o,s,a,s]).
palabra([r,a,n,a]).
palabra([a,s,e,s]).
palabra([m,e,r,a]).
palabra([e,s,a,s]).
palabra([s,a,n,e]).
palabra([a,s,a,s]).
palabra([c,a,s,a]).
palabra([m,o,r,a]).

/*
pertenece(?X,?L) ← El elemento X pertenece a la lista L.
Ej.: pertenece(a,[a,b,c,a]).
*/
pertenece(X,[X|_]).
pertenece(X,[Y|L]):-
    X \= Y,
    pertenece(X,L).
/*
Predicado auxiliar
*/
no_pertenece(_,[]).
no_pertenece(X,[Y|L]):-
    X \= Y,
    no_pertenece(X,L).

/*
unico(+X,+L) ← El elemento X tiene una única ocurrencia en la lista L.
Ej.: unico(b,[a,b,c,a]).
*/
unico(X,[X|L]):- 
    no_pertenece(X,L).
unico(X,[Y|L]):-
    X \= Y,
    unico(X,L).

/*
elegir_primero(+X,+L1,?L2) ← La lista L2 contiene los elementos de L1 sin la primera
ocurrencia de X, si X pertenece a L2.
Ej: elegir_primero(a,[b,a,c,a,a],[b,c,a,a]).
 elegir_primero(d,[b,a,c,a,a],[b,a,c,a,a]).
*/
elegir_primero(X, [X|R1], R1).
elegir_primero(X, [Y|R1], [Y|R2]):-
    elegir_primero(X,R1,R2).

/*
repetido(+X,?L) ← El elemento X tiene más de una ocurrencia en la lista L.
Ej: repetido(a,[a,b,c,a]).
*/
repetido(X,[X|L]):- 
    pertenece(X,L).

repetido(X,[Y|L]):- 
    X \= Y,
    repetido(X,L).
    

/*
pertenece_veces(+X,+L,?N) ← El elemento X ocurre N veces en la lista L.
Ej.: pertenece(a,[a,b,c,a]).
*/
pertenece_veces_acc(_,[],Ac,Ac).
pertenece_veces_acc(X,[X|L],Ac,N):-
    Ac1 is Ac + 1,
    pertenece_veces_acc(X,L,Ac1,N).
pertenece_veces_acc(X,[Y|L],Ac,N):-
    X \= Y,
    pertenece_veces_acc(X,L,Ac,N).
    
pertenece_veces(X,L,N):-
    pertenece_veces_acc(X,L,0,N).

/*
Predicado auxiliar
*/
appendear([], X, X).
appendear([X|Y], Z, [X|W]) :- appendear(Y,Z,W).

/*
pares(+L1,?L2) ← L2 es la lista que contiene los elementos pares de L1.
Ej: pares([1,2,3,4,5],[2,4]).
*/
pares_acc([], Acc, Acc).
pares_acc([X|L1], Acc, L2):-
    Aux is X mod 2,
    Aux = 0,
    appendear(Acc, [X], Acc2),
    pares_acc(L1, Acc2, L2).

pares_acc([X|L1], Acc, L2):-
    Aux is X mod 2,
    Aux = 1,
    pares_acc(L1, Acc, L2).

pares(L1, L2):-
    pares_acc(L1, [], L2).

/*
pares_impares(+L1,?L2,?L3) ← L2 es una lista con los valores pares de la lista L1,
L3 es una lista con los valores impares de la lista L1.
Ej.: pares_impares([4,5,3,1,2],[4,2],[5,3,1]).
*/
pares_impares_acc([], Acc2, Acc3, Acc2, Acc3).
pares_impares_acc([X|L1], Acc2, Acc3, L2, L3):-
    Aux is X mod 2,
    Aux = 0,
    appendear(Acc2, [X], AccAux),
    pares_impares_acc(L1, AccAux, Acc3, L2, L3).

pares_impares_acc([X|L1], Acc2, Acc3, L2, L3):-
    Aux is X mod 2,
    Aux = 1,
    appendear(Acc3, [X], AccAux),
    pares_impares_acc(L1, Acc2, AccAux, L2, L3).

pares_impares(L1, L2, L3):-
    pares_impares_acc(L1, [], [], L2, L3).

/*
ordenada(+L1,?L2) ← L2 contiene los elementos de L1 ordenados de menor a mayor,
utilizando el algoritmo de ordenación por selección. Las listas contienen valores enteros y no
hay elementos repetidos.
Ej.: ordenada([4,5,3,1,2],[1,2,3,4,5]).
*/
mas_chico_acc([],Acc,Acc).
mas_chico_acc([X|L],Acc,N):-
    X < Acc,
    mas_chico_acc(L,X,N).

mas_chico_acc([X|L],Acc,N):-
    X >= Acc,
    mas_chico_acc(L,Acc,N).

mas_chico([X|L],N):-
    mas_chico_acc(L,X,N).

ordenada([X],[X]).
ordenada(L1,[X|L2]):-
    mas_chico(L1,X),
    elegir_primero(X,L1,L1next),
    ordenada(L1next,L2).
