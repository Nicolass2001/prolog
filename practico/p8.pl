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

/* Ejercicio 4 [Fundamental]
Considere la representación de matrices mediante functores en Prolog. Una matriz
se representa como un functor m aplicado a una serie de filas, y cada fila es un
functor f aplicado a una serie de celdas. Por ejemplo, la siguiente matriz de tamaño
2x3:
1 2 3
4 5 6
se representa como: m(f(1, 2, 3),f(4, 5, 6))
Implemente los siguientes predicados:

matriz(+F,+C,+V,-M) ← M es una matriz de F filas y C columnas donde cada celda tiene el valor V

celda(+M,?I,?J,?V) ← V es el valor de la celda (I,J) de la matriz M

nuevo_valor(+M,+I, +J,+V) ← Se sustituye el valor de la celda (I,J) de la matriz M por V*

suma(+M,+N,?S) ← S es la suma de las matrices M y N

Notar que nuevo_valor no tiene argumento de salida. Se sugiere investigar el
predicado extralógico set_arg/3 de SWI-Prolog.
*/

% matriz(+F,+C,+V,-M) ← M es una matriz de F filas y C columnas donde cada celda tiene el valor V
generar_lista(0,_,[]):- !.
generar_lista(N,V,[V|Lista]):-
    NNew is N - 1,
    generar_lista(NNew,V,Lista).

generar_lista_filas(0,_,_,[]):- !.
generar_lista_filas(F,C,V,[Fila|ListaFilas]):-
    generar_lista(C,V,ListaFila),
    Fila =.. [f|ListaFila],
    FNew is F - 1,
    generar_lista_filas(FNew,C,V,ListaFilas).

matriz(F,C,V,M):-
    generar_lista_filas(F,C,V,ListaFilas),
    M =.. [m|ListaFilas].

% celda(+M,?I,?J,?V) ← V es el valor de la celda (I,J) de la matriz M
celda(M,I,J,V):-
    M =.. [_|Filas],
    nth1(I,Filas,Fila),
    Fila =.. [_|Valores],
    nth1(J,Valores,V).

% nuevo_valor(+M,+I, +J,+V) ← Se sustituye el valor de la celda (I,J) de la matriz M por V*
nuevo_valor(M,I,J,V):-
    M =.. [_|Filas],
    nth1(I,Filas,Fila),
    setarg(J,Fila,V).

% suma(+M,+N,?S) ← S es la suma de las matrices M y N
suma_valores([],[],[]):- !.
suma_valores([Vm|M],[Vn|N],[Vs|S]):-
    Vs is Vm + Vn,
    suma_valores(M,N,S).


suma_filas(M,N,S):-
    M =.. [_|ValoresM],
    N =.. [_|ValoresN],
    length(ValoresM,F),
    length(ValoresN,F),
    suma_valores(ValoresM,ValoresN,ValoresS),
    S =.. [f|ValoresS].

suma_filas_aux([],[],[]):- !.
suma_filas_aux([Fm|M],[Fn|N],[Fs|S]):-
    suma_filas(Fm,Fn,Fs),
    suma_filas_aux(M,N,S).

suma_matriz(M,N,S):-
    M =.. [_|FilasM],
    N =.. [_|FilasN],
    length(FilasM,F),
    length(FilasN,F),
    suma_filas_aux(FilasM,FilasN,FilasS),
    S =.. [m|FilasS].

/* Ejercicio 5 [Fundamental]
Sea el siguiente programa Prolog:
    padre(juan, ana).
    padre(juan, jose).
    padre(juan, pedro).
    padre(pedro, hector).
    padre(pedro, gustavo).
    padre(hector, maria).
Indique las respuestas que se obtienen para L con los siguientes objetivos:
    i. findall(X, padre(juan,X), L).
    ii. findall(X, padre(Y,X), L).
    iii. findall(X, (padre(juan,X) ; padre(pedro,X)), L).
    iv. findall(X, (padre(juan,X), padre(X,Y)), L).
    v. setof(X, padre(juan,X), L).
    vi. setof(X, padre(Y,X), L).
    vii. setof(X, (padre(juan,X) ; padre(pedro,X)), L).
    viii. setof(X, (padre(juan,X), padre(X,Y)), L).
*/

/* Ejercicio 6 [Fundamental]
Utilizando predicados de segundo orden, implemente los siguientes predicados:

pares(+L,?P) ← P contiene los elementos pares de L

mayores(+L,+X,?M) ← M contiene los elementos de L que son mayores que X.

union(+C1,+C2,?C) ← C es la unión de los conjuntos C1 y C2.

interseccion(+C1,+C2,?C) ← C es la intersección de los conjuntos C1 y C2.

diferencia(+C1,+C2,-C) ← C es igual a C1-C2.

adyacentes(+N,?A) ← A es la lista de nodos adyacentes al nodo N en un grafo 
definido mediante el predicado arista(N1, N2).

max_comun(+L1,+L2,?L) ← L es la sublista más larga común a L1 y L2
*/

/* Ejercicio 7 [Fundamental]
Implemente los siguientes predicados de segundo orden. Asuma que los
argumentos U, B y T contendrán predicados unarios (por ejemplo par/1), binarios
(por ejemplo doble/2) o ternarios (por ejemplo suma/3) respectivamente.

any(+L,+U) ← Algún elemento de L cumple la propiedad U.

all(+L,+U) ← Todos los elementos de L cumplen la propiedad U

map(+L,+B,?L2) ← L2 es el resultado de aplicar la función B a todos los elementos de L

combine(+L1,+L2,+T,?L3) ← L3 es el resultado de aplicar el operador T a elementos 
en las mismas posiciones de L1 y L2

fold(+L,+T,?F) ← F es el resultado de realizar un fold sobre la lista L con el operador T. 
Por ejemplo, si T fuera la suma la operación sería: F = L1 + L2 + … + Ln-1 + Ln
*/