/* Ejercicio 1 [Fundamental]
Escriba los siguientes predicados sobre listas de diferencias.
Para las listas de diferencias utilice la representación mediante términos
estructurados de la forma: X-Xr
*/
/*
l_ld(L, LD) LD es una lista de diferencias equivalente a la lista L.
*/
l_ld([],LDr-LDr).
l_ld([X|L],[X|LD]-LDr):-
    l_ld(L,LD-LDr).

/*
ld_l(LD, L) L es la lista equivalente a la lista de diferencias LD.
*/
ld_l(LD-[],LD).

/*
append_ld(A,B,C) C es la lista de diferencia equivalente a concatenar
las listas de diferencias A y B.
*/
append_ld(A-B,B-Br,A-Br).

/*
inserta_ld(L1,X,L2) L2 es la lista de diferencias resultante de insertar el
elemento X al comienzo de la lista de diferencias L1.
*/
inserta_ld(L1-L1r,X,[X|L1]-L1r).

/*
insertz_ld(L1,X,L2) L2 es la lista de diferencias resultante de insertar el
elemento X al final de la lista de diferencias L1.
*/
insertz_ld(L1-[X|L2r],X,L1-L2r).

/*
rotacion_ld(L1, L2) L2 es la lista de diferencias que resulta de rotar un
lugar a la izquierda, en forma circular, los elementos
de la lista de diferencias L1.
Ejemplo: rotacion_ld([a,b,c|X]-X, [b,c,a|Y]-Y)
*/
rotacion_ld([X|L1]-[X|L2r],L1-L2r).

/*
reverse_ld(L,R) R es la lista de diferencias que representa el inverso
de la lista común L.
*/
reverse_ld([X|L]-L,[X|R]-R):-
    var(L), !.
reverse_ld([X|L]-Lr,R-Rr):-
    reverse_ld(L-Lr,R-[X|Rr]).

/*
quicksort_ld(L,S) S es la lista de diferencias ordenada que representa
la lista común L ordenada utilizando el algoritmo
quicksort.
*/
quicksort_ld([],S-S).
quicksort_ld([X|L],S-Sr):-
    quicksort_ld_ordenar(X,L,S1Aux,S2Aux),
    quicksort_ld(S1Aux,S1-S1r),
    quicksort_ld(S2Aux,S2-S2r),
    insertz_ld(S1-S1r,X,S1X-S1Xr),
    append_ld(S1X-S1Xr,S2-S2r,S-Sr).

quicksort_ld_ordenar(_,[],[],[]):- !.
quicksort_ld_ordenar(X,[Y|L],[Y|S1],S2):-
    X < Y, !,
    quicksort_ld_ordenar(X,L,S1,S2).
quicksort_ld_ordenar(X,[Y|L],S1,[Y|S2]):-
    quicksort_ld_ordenar(X,L,S1,S2).

/* Ejercicio 3 [Fundamental]
Escriba los siguientes predicados para árboles de enteros, representados mediante
estructuras incompletas:
*/
/*
pre_orden(A, L) L es una lista con los elementos del árbol binario A,
obtenida al recorrerlo pre-orden.
*/
pre_orden(A,L-L):-
    var(A), !.
pre_orden(a(E,D,I),[E|IL]-DLr):-
    pre_orden(I,IL-ILr),
    pre_orden(D,ILr-DLr).

/*
in_orden(A, L) L es una lista con los elementos del árbol binario A,
obtenida al recorrerlo in-orden.
*/
in_orden(A,L-L):-
    var(A), !.
in_orden(a(E,D,I),IL-DLr):-
    in_orden(I,IL-[E|ILr]),
    in_orden(D,ILr-DLr).

/*
ins_abb(A, E) El árbol binario de búsqueda A contiene al
elemento E en la posición que le corresponde
según su valor.
*/
ins_abb(A,E):-
    var(A),
    A = a(E,_,_), 
    !.
ins_abb(a(N,D,_),E):-
    N > E,
    ins_abb(D,E), !.
ins_abb(a(_,_,I),E):-
    ins_abb(I,E).

/* Ejercicio 4 [Fundamental]
Implemente los siguientes predicados sobre listas de diferencias en Prolog:
*/
/*
a) [prueba 2020]
largo_ld(+L,?N) ← N es el largo de los elementos de la lista de diferencias L, sin
contar el resto variable. Por ejemplo:
largo_ld([a,b,c,d|LR]-LR,4).
largo_ld([c,d|LR]-LR,2).
*/
largo_ld(L-L,0):-
    var(L), !.
largo_ld([_|L]-Lr,Nnew):-
    largo_ld(L-Lr,N),
    Nnew is N + 1.

/*
b) [prueba 2021]
not_member_ld(+X,?L) ← X es un elemento que no está presente en la lista de
diferencias L utilizada con la notación L-LR. Por ejemplo:
not_member_ld(6,[1,2,3,4|LR]-LR). ← Devuelve “true”.
not_member_ld(4,[1,2,3,4|LR]-LR). ← Devuelve “false”.
*/
not_member_ld(_,L-L):-
    var(L), !.
not_member_ld(X,[Y|L]-Lr):-
    X \= Y,
    not_member_ld(X,L-Lr).

/* Ejercicio 5 [Fundamental]
Utilizando DCG, defina programas Prolog para reconocer los siguientes lenguajes:
*/
% L = {a* b* c* }
l1 --> a1, b1, c1.
a1 --> [].
a1 --> [a], a1.
b1 --> [].
b1 --> [b], b1.
c1 --> [].
c1 --> [c], c1.

% L = {a^n b^n / n ≥ 0}
l2 --> a2(N), b2(N).
a2(0) --> [].
a2(N) --> [a], a2(NPrev), {N is NPrev + 1}.
b2(0) --> [].
b2(N) --> [b], b2(NPrev), {N is NPrev + 1}.
c2(0) --> [].
c2(N) --> [c], c2(NPrev), {N is NPrev + 1}.

% L = {wwR / w ∈ {a,b}* }
l3 --> [].
l3 --> [a], l3, [a].
l3 --> [b], l3, [b].

% L = {a^n b^n c^n / n ≥ 0}
l4 --> a2(N), b2(N), c2(N).

% L = {a^n b^m c^n+m / n,m ≥ 0}
% l5 --> a2(N), b2(M), {NM is N + M}, c2(NM).
l5 --> [].
l5 --> [a], l5, [c].
l5 --> [a], bc, [c].
bc --> [].
bc --> [b], bc, [c].

% L = {a^p b^m c^p*m / p,m ≥ 0}
l6 --> a2(N), b2(M), {NM is N * M}, c2(NM).

/* Ejercicio 6 [Fundamental]
a) Escriba una gramática en Prolog, usando la notación DCG, que permita
reconocer frases como:

Los osos polares comen peces.
Los osos polares viven en la Antártida.
El investigador da alimento a los osos polares.
Los investigadores estudian el comportamiento de las aves.

b) Explique cómo haría para no permitir frases como:
Las investigador estudian el comportamiento de los aves.

c) Explique cómo haría para no permitir frases como:
Los osos polares comen la Antártida.
*/

/* Ejercicio 7 [Fundamental]

a) [prueba 2010] Defina una gramática DCG para el lenguaje sobre el alfabeto {a,b}
cuyas tiras son de la forma ww , w ∈ {a,b}*
*/
% Definición principal: una cadena de la forma ww
la --> w(W), w(W).
w([]) --> [].
w([a|T]) --> [a], w(T).
w([b|T]) --> [b], w(T).

/*
b) [prueba 2012] Construya una gramática DCG que reconozca el lenguaje L={y,w ∈
{a,b}/ x=ywwRy}.
*/
lb --> la.
lb --> [a], lb, [a].
lb --> [b], lb, [b].
