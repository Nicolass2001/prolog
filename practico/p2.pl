/* Ejercicio 1 ----------------------------------------------------------------
Indique la respuesta que daría Prolog para los siguientes objetivos:
i) X is 10 + 5. 
ii) 10 + 5 is X. 
iii) 10 + 5 is 10 + 5. 
iv) Y = 5, X is Y + 1. 
v) X = 5, X is X + 1. 
vi) X = 1, X > 0. 
vii) X < X + 1. 
viii) X > 0, X = 1. 
ix) 10 + 5 =:= 10 + 5.
x) 20 - 5 =:= 10 + 5.
xi) 20 - 5 = 10 + 5.
xii) X \= 10.
xiii) X = 5, X \= 10.
xiv) X =\= 10.
xv) X = 5, X =\= 10.
xvi) X \= Y.
*/
/*
i) X is 10 + 5.
X = 15.

ii) 10 + 5 is X.
false.

iii) 10 + 5 is 10 + 5.
false.

iv) Y = 5, X is Y + 1.
Y = 5,
X = 6.

v) X = 5, X is X + 1.
false.

vi) X = 1, X > 0.
X = 1.

vii) X < X + 1.
error. (variable no instanciada)

viii) X > 0, X = 1.
error. (variable no instanciada)

ix) 10 + 5 =:= 10 + 5.
true.

x) 20 - 5 =:= 10 + 5.
true.

xi) 20 - 5 = 10 + 5.
false.

xii) X \= 10.
false.

xiii) X = 5, X \= 10.
X = 5.

xiv) X =\= 10.
error. (variable no instanciada)

xv) X = 5, X =\= 10.
X = 5.

xvi) X \= Y.
false.

*/

/* Ejercicio 2 ----------------------------------------------------------------
a) Defina los siguientes predicados en Prolog sin utilizar acumuladores:
    largo(+L,?N) N es el largo de la lista L
    máximo(+L,?M) M es el máximo elemento de la lista L

    + significa que la variable tiene q estar instanciada
    ? significa que puede estar o no instanciada (es lo mismo q no poner nada)
b) Defina los predicados de la parte a) utilizando la técnica de acumuladores.
c) Utilizando el predicado de sistema time/1, ejecute los predicados de las partes a)
y b) para listas de diferentes largos. Compare la eficiencia de las dos versiones de
los predicados en cuanto a cantidad de inferencias y tiempo de ejecución.
*/

% a)
% largo(+L,?N) N es el largo de la lista L
largo([],0).
largo([_|L],N):- largo(L,X), N is X + 1.

% máximo(+L,?M) M es el máximo elemento de la lista L
maximo([X],X).
maximo([X|L],N):- maximo(L,N), N >= X.
maximo([X|L],X):- maximo(L,N), X > N.
/* Usando trampilla.
maximo([X|L],M):- maximo(L,N), M is max(N,X).
*/

% b)
% largo2(+L,?N) N es el largo de la lista L
largo_acc([],Ac,Ac).
largo_acc([_|L],N,Ac):- Ac1 is Ac + 1, largo_acc(L,N,Ac1).

largo2(L,N):- largo_acc(L,N,0).

% máximo2(+L,?M) M es el máximo elemento de la lista L
maximo_acc([],Ac,Ac).
maximo_acc([X|L],N,Ac):- X > Ac, maximo_acc(L,N,X).
maximo_acc([X|L],N,Ac):- X =< Ac, maximo_acc(L,N,Ac).

maximo2([X|L],M):- maximo_acc(L,M,X).

% c)
/* 
time(largo([1,2,5,3,6,1,3], Z)).
% 13 inferences, 0.000 CPU in 0.000 seconds (70% CPU, 1444444 Lips)
Z = 7.

time(largo2([1,2,5,3,6,1,3], Z)).
% 7 inferences, 0.000 CPU in 0.000 seconds (62% CPU, 250000 Lips)
Z = 7.

time(maximo([1,2,5,3,6,1,3], Z)).
% 20 inferences, 0.000 CPU in 0.000 seconds (72% CPU, 645161 Lips)
Z = 6 ;
% 363 inferences, 0.000 CPU in 0.000 seconds (91% CPU, 2372549 Lips)
false.

time(maximo2([1,2,5,3,6,1,3], Z)).
% 15 inferences, 0.000 CPU in 0.000 seconds (48% CPU, 1153846 Lips)
Z = 6 ;
% 9 inferences, 0.000 CPU in 0.000 seconds (61% CPU, 321429 Lips)
false.

20 inferences: Prolog hizo 20 pasos de inferencia para encontrar la primera solución.
0.000 CPU in 0.000 seconds: El tiempo de CPU fue insignificante.
(72% CPU, 645161 Lips):
72% CPU: Indica cuánto del tiempo de CPU se usó.
645161 Lips: "Logical Inferences Per Second" (inferencias lógicas por segundo).
*/


/* Ejercicio 3 ----------------------------------------------------------------
Defina los siguientes predicados en Prolog:
suma(+L,?S) S es la suma de los elementos de la lista L
pares(+L,?P) P es una lista conteniendo solo los elementos pares de la lista L 
mayores(+L,+X,?M) M es una lista con los elementos de L que son mayores que X
merge(+L1,+L2,?L3) L3 es el resultado de combinar ordenadamente los elementos 
de las listas (ordenadas) L1 y L2
*/

% suma(+L,?S) S es la suma de los elementos de la lista L
suma([],0).
suma([X|L],S):- suma(L,S1), S is S1 + X.

% pares(+L,?P) P es una lista conteniendo solo los elementos pares de la lista L
pares([],[]).
pares([X|L],P):-  mod(X,2) =\= 0, pares(L,P). 
pares([X|L],[X|P]):- mod(X,2) =:= 0, pares(L,P).  

% mayores(+L,+X,?M) M es una lista con los elementos de L que son mayores que X
mayores_acc([],_,Ac,Ac).
mayores_acc([Y|L],X,Ac,M):- Y > X, mayores_acc(L,X,[Y|Ac],M).
mayores_acc([Y|L],X,Ac,M):- Y =< X, mayores_acc(L,X,Ac,M).
mayores(L,X,M):- mayores_acc(L,X,[],M).

% merge(+L1,+L2,?L3) L3 es el resultado de combinar ordenadamente los elementos
merge_acc([],[],Ac,Ac).
merge_acc(L1,[],Ac,L3):- append(Ac,L1,L3).
merge_acc([],L2,Ac,L3):- append(Ac,L2,L3).
merge_acc([X|L1],[Y|L2],Ac,L3):- X < Y, append(Ac,[X],Ac1), merge_acc(L1,[Y|L2],Ac1,L3).
merge_acc([X|L1],[Y|L2],Ac,L3):- X >= Y, append(Ac,[Y],Ac1), merge_acc([X|L1],L2,Ac1,L3).

merge(L1,L2,L3):- merge_acc(L1,L2,[],L3).
/* Sin acumulador
merge1(L1,[],L1).
merge1([],L2,L2).
merge1([X|L1],[Y|L2],[X|L3]):- X < Y, merge1(L1,[Y|L2],L3).
merge1([X|L1],[Y|L2],[Y|L3]):- X >= Y, merge1([X|L1],L2,L3).
*/

/* Ejercicio 4 ----------------------------------------------------------------
a) Defina los siguientes predicados en Prolog:

insertionsort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo insertion sort
mergesort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo merge sort
quicksort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo quick sort

b) Utilizando el predicado de sistema time/1, ejecute los predicados de la parte a)
para listas de diferentes largos. Compare la performance de los tres algoritmos en
cuanto a cantidad de inferencias y tiempo de ejecución.
*/

% a)
% insertionsort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo insertion sort
insertar(X,[],[X]).
insertar(X,[Y|S1],[X|[Y|S1]]):- Y >= X.
insertar(X,[Y|S1],[Y|S2]):- Y < X, insertar(X,S1,S2).
/*
insertionsort([],[]).
insertionsort([X|L],S):- insertionsort(L,S1), insertar(X,S1,S).
*/
insort([], Ac, Ac).
insort([H|T],  A, S) :- insertar(H,A,A1), insort(T,A1,S).
insertionsort(L,S):- insort(L,[],S).

% mergesort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo merge sort
% (Solucion internet)
divide([], [], []).
divide([A], [A], []).
divide([A, B | R], [A | Ra], [B | Rb]) :-  divide(R, Ra, Rb).

my_merge(A, [], A).
my_merge([], B, B).
my_merge([A | Ra], [B | Rb], [A | M]) :- A =< B, my_merge(Ra, [B | Rb], M).
my_merge([A | Ra], [B | Rb], [B | M]) :- A > B, my_merge([A | Ra], Rb, M).

mergesort([],[]).
mergesort([A], [A]).
mergesort([A, B | Rest], S) :- divide([A, B | Rest], L1, L2), mergesort(L1, S1), mergesort(L2, S2), my_merge(S1, S2, S).

% quicksort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo quick sort
pivot(_,[],[],[]).
pivot(X,[Y|L],[Y|L1],L2):- Y =< X, pivot(X,L,L1,L2).
pivot(X,[Y|L],L1,[Y|L2]):- Y > X, pivot(X,L,L1,L2).

quicksort([],[]).
quicksort([X|L],S):- pivot(X,L,L1,L2), quicksort(L1,L3), quicksort(L2,L4), append(L3,[X],L5), append(L5,L4,S).

/* Ejercicio 5 ----------------------------------------------------------------
Considere la representación de vectores mediante listas de valores reales en Prolog.
Implemente los siguientes predicados:

neg(+V,?W) W es el vector opuesto a V
suma(+V,+W,?T) T es la suma de los vectores V y W
dot(+V,+W,?P) P es el producto punto entre V y W
dist(+V,+W,?D) D es la distancia euclídea entre V y W
*/
% neg(+V,?W) W es el vector opuesto a V
neg([],[]).
neg([X|V],[Y|W]):- Y is -X, neg(V,W).

% suma(+V,+W,?T) T es la suma de los vectores V y W
suma_vector([],[],[]).
suma_vector([X|V],[Y|W],[Z|T]):- Z is X + Y, suma_vector(V,W,T).  

% dot(+V,+W,?P) P es el producto punto entre V y W 
dot([],[],0).
dot([X|V],[Y|W],P):- dot(V,W,R), P is X*Y+R.

% dist(+V,+W,?D) D es la distancia euclídea entre V y W
dist_aux([],[],0).
dist_aux([X|V],[Y|W],D):- dist_aux(V,W,Z), D is (X-Y)^2 + Z.
dist(V,W,D):- dist_aux(V,W,X), D is sqrt(X).

/* Ejercicio 6 ----------------------------------------------------------------
Considere la representación de matrices mediante listas de listas de valores reales en Prolog.
Por ejemplo, la siguiente matrix de tamaño 2x3:
1 2 3
4 5 6
se representa como: [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
Implemente los siguientes predicados:
columna(+M,?C,?R) C es la primera columna de M en forma de lista, R es M sin la primera columna
transpuesta(+M,?T) T es la transpuesta de la matriz T 
simetrica(+M) M es una matriz simétrica
suma(+M,+N,?S) S es la suma de las matrices M y N 
producto(+M,+N,?P) P es el producto de las matrices M y N
*/
% columna(+M,?C,?R) C es la primera columna de M en forma de lista, R es M sin la primera columna
columna([],[],[]).
columna([[X|V]|M],[X|C],[V|R]):- columna(M,C,R).

% transpuesta(+M,?T) T es la transpuesta de la matriz T
% [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ] -> [[ 1, 4, 7 ], [ 2, 5, 8 ], [ 3, 6, 9 ]]
transpuesta([],[]).
transpuesta([[]|_],[]).
transpuesta(M,[C|T]):- columna(M,C,R), transpuesta(R,T).

% simetrica(+M) M es una matriz simétrica
% [ [ 1, 2, 3 ], [ 2, 4, 5 ], [ 3, 5, 6 ] ] -> true
simetrica(M):- transpuesta(M,M).

% suma(+M,+N,?S) S es la suma de las matrices M y N
suma([],[],[]).
suma([[]|M],[[]|N],[[]|S]):- suma(M,N,S).
suma([[X|Rx]|M],[[Y|Ry]|N],[[Z|Rz]|S]):- Z is X + Y, suma([Rx|M],[Ry|N],[Rz|S]).

% producto(+M,+N,?P) P es el producto de las matrices M y N
producto_v(_,[[]|_],[]).
producto_v(V,N,[Z|P]):- columna(N,C,R), dot(V,C,Z), producto_v(V,R,P).
producto([],_,[]).
producto([X|M],N,[Z|P]):- producto_v(X,N,Z), producto(M,N,P).
