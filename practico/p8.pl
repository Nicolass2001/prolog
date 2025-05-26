/* Ejercicio 1
Implemente el predicado largo(L,N) de manera que funcione para cualquier
instanciación.
*/

largo_1([],0).
largo_1([_|L],NNew):-
    largo(L,N),
    NNew is N + 1.

largo_2([],0).
largo_2([_|L],N):-
    NNew is N - 1,
    largo_2(L,NNew).

largo(L,N):-
    nonvar(N),
    largo_2(L,N),
    !.
largo(L,N):-
    largo_1(L,N).

/* Ejercicio 2 [Fundamental]

a) Implemente los siguientes predicados en Prolog sobre valores reales de manera
que puedan invocarse para cualquier instanciación, siempre que la cantidad de
argumentos no instanciados sea como máximo 1.

suma(X, Y, Z) ← Z es la suma entre X e Y.

producto(X, Y, Z) ← Z es el producto entre X e Y

cuadrado(X, Y) ← Y es el cuadrado de X. Notar que para la invocación (-X,+Y) 
pueden existir 0, 1 o 2 valoresde X posibles, dependiendo de Y.

exponencial(X, Y, Z) ← Z es X^Y.

b) Implemente el predicado suma(X, Y, Z) sobre valores enteros que funcione para
las instanciaciones de la parte a), y además para la instanciación (-X,-Y, +Z)
considerando que X e Y serán mayores o iguales que 0.
*/

% suma(X, Y, Z) ← Z es la suma entre X e Y.
suma(X, Y, Z):-
    var(Z),
    nonvar(X),
    nonvar(Y),
    Z is X + Y, !.

suma(X, Y, Z):-
    var(X),
    nonvar(Z),
    nonvar(Y),
    X is Z - Y, !.

suma(X, Y, Z):-
    nonvar(X),
    nonvar(Z),
    Y is Z - X.

% producto(X, Y, Z) ← Z es el producto entre X e Y
producto(X, Y, Z):-
    var(Z),
    nonvar(X),
    nonvar(Y),
    Z is X * Y, !.

producto(X, Y, Z):-
    var(X),
    nonvar(Z),
    nonvar(Y),
    X is Z / Y, !.

producto(X, Y, Z):-
    nonvar(X),
    nonvar(Z),
    Y is Z / X.

% cuadrado(X, Y) ← Y es el cuadrado de X. Notar que para la invocación (-X,+Y) 
% pueden existir 0, 1 o 2 valoresde X posibles, dependiendo de Y.
cuadrado(X, Y):-
    var(Y),
    nonvar(X),
    Y is X * X, !.

cuadrado(X, Y):-
    nonvar(Y),
    Y > 0,
    X is sqrt(Y).

cuadrado(X, Y):-
    nonvar(Y),
    Y > 0,
    X is -sqrt(Y).

% exponencial(X, Y, Z) ← Z es X^Y.
exponencial(X, Y, Z):-
    var(X),
    nonvar(Z),
    nonvar(Y),
    X is Z ^ (1/Y), !.

exponencial(X, Y, Z):-
    var(Y),
    nonvar(X),
    nonvar(Z),
    Y is log(Z) / log(X), !.

exponencial(X, Y, Z):-
    nonvar(X),
    nonvar(Y),
    Z is X ^ Y, !.

% b) Implemente el predicado suma(X, Y, Z) sobre valores enteros que funcione para
% las instanciaciones de la parte a), y además para la instanciación (-X,-Y, +Z)
% considerando que X e Y serán mayores o iguales que 0.
sumab(X, Y, Z):-
    var(Z),
    nonvar(X),
    nonvar(Y),
    Z is X + Y, !.

sumab(X, Y, Z):-
    var(X),
    nonvar(Z),
    nonvar(Y),
    X is Z - Y, !.

sumab(X, Y, Z):-
    nonvar(X),
    nonvar(Z),
    Y is Z - X, !.

sumab(X, Y, Z):-
    var(X),
    var(Y),
    nonvar(Z),
    between(0, Z, X),
    Y is Z - X.
