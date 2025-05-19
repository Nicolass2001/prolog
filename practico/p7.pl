/* Ejercicio 1 [Fundamental]
a) Considere que C1, C2 y C3 son conjuntos representados como listas sin
elementos repetidos. Implemente los siguientes predicados en Prolog puro más el
predicado \= para chequear que dos elementos sean diferentes.

i. intersec(+C1,+C2,?C3) ← C3 es la intersección de los conjuntos C1 y C2.
ii. diferencia(+C1,+C2,?C3) ← C3 es el conjunto C1 - C2.
*/

/* i. intersec(+C1,+C2,?C3) ← C3 es la intersección de los conjuntos C1 y C2. 
intersec([1,2,3],[3,4,5],C3). => [3]
intersec([1,2,3,4,5,11,12,13,14,15,16,17,18,19,20],[3,4,5,6,7,8,9,10,11,12,13,21,22,23,24],C3). => [3, 4, 5, 11, 12, 13]
intersec([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],C3). => [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
*/
pertenece(X,[X|_]).
pertenece(X,[Y|C2]):-
    X \= Y,
    pertenece(X,C2).
no_pertenece(_,[]).
no_pertenece(X,[Y|C2]):-
    X \= Y,
    no_pertenece(X,C2).
intersec([],_,[]).
intersec([X|C1],C2,[X|C3]):-
    pertenece(X,C2),
    !,
    intersec(C1,C2,C3).
intersec([X|C1],C2,C3):-
    no_pertenece(X,C2),
    intersec(C1,C2,C3).

/* ii. diferencia(+C1,+C2,?C3) ← C3 es el conjunto C1 - C2. 
diferencia([1,2,3],[3,4,5],C3). => [1,2]
diferencia([1,2,3,4,5,11,12,13,14,15,16,17,18,19,20],[3,4,5,6,7,8,9,10,11,12,13,21,22,23,24],C3). => [1, 2, 14, 15, 16, 17, 18, 19, 20]
diferencia([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],C3). => [1]
*/
diferencia([],_,[]).
diferencia([X|C1],C2,C3):-
    pertenece(X,C2),
    !,
    diferencia(C1,C2,C3).
diferencia([X|C1],C2,[X|C3]):-
    no_pertenece(X,C2),
    diferencia(C1,C2,C3).

/* b) Mejore la eficiencia de los predicados de la parte a) utilizando cut de manera que
no se recorra la segunda lista lista innecesariamente.

Se agrega linea 26 y 40.
*/

/* Ejercicio 7
Considere que C1, C2 y C3 son conjuntos representados como listas sin elementos
repetidos. Implemente los siguientes predicados en Prolog utilizando not:
i. diferencia(+C1,+C2,?C3) ← C3 es el conjunto C1 - C2.
ii. disjuntos(+C1,+C2) ← C1 y C2 son disjuntos.
*/

/* i. diferencia(+C1,+C2,?C3) ← C3 es el conjunto C1 - C2.
diferencia_not([1,2,3],[3,4,5],C3). => [1,2]
diferencia_not([1,2,3,4,5,11,12,13,14,15,16,17,18,19,20],[3,4,5,6,7,8,9,10,11,12,13,21,22,23,24],C3). => [1, 2, 14, 15, 16, 17, 18, 19, 20]
diferencia_not([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],C3). => [1]
*/
diferencia_not([],_,[]).
diferencia_not([X|C1],C2,[X|C3]):-
    not(member(X,C2)),
    diferencia_not(C1,C2,C3).
diferencia_not([X|C1],C2,C3):-
    member(X,C2),
    diferencia_not(C1,C2,C3).

/* ii. disjuntos(+C1,+C2) ← C1 y C2 son disjuntos.
disjuntos([1,2,3,4],[5,6,7,8]).
true.
disjuntos([1,2,3,4],[1,2,3,4]).
false.
disjuntos([1,2,3,4],[5,3,7,8]).
false
*/
disjuntos([],_).
disjuntos([X|C1],C2):-
    not(member(X,C2)),
    disjuntos(C1,C2).