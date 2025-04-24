
% ############################################################################
%                         PARTE 1: Predicados varios
% ############################################################################

/*
pertenece(?X,?L) ← El elemento X pertenece a la lista L.
Ej.: pertenece(a,[a,b,c,a]).
*/
pertenece(X,[X|_]).
pertenece(X,[_|L]):-
    pertenece(X,L).

/*
Predicado auxiliar
no_pertenece(+X,+L) ← El elemento X no pertenece a la lista L.
Ej.: no_pertenece(d,[a,b,c,a]).
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
    X \= Y,
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
pertenece_veces_acc(+X,+L,+Ac,?N) ← El elemento X ocurre N veces en la lista L, usando Acumulador Ac (inicializado en 0).
Ej.: pertenece_veces_acc(a,[a,b,c,a],0,2).
*/

pertenece_veces_acc(_,[],Ac,Ac).
pertenece_veces_acc(X,[X|L],Ac,N):-
    Ac1 is Ac + 1,
    pertenece_veces_acc(X,L,Ac1,N).
pertenece_veces_acc(X,[Y|L],Ac,N):-
    X \= Y,
    pertenece_veces_acc(X,L,Ac,N).

/*
pertenece_veces(+X,+L,?N) ← El elemento X ocurre N veces en la lista L.
Ej.: pertenece_veces(a,[a,b,c,a],2).
*/

pertenece_veces(X,L,N):-
    pertenece_veces_acc(X,L,0,N).

/*
Predicado auxiliar
appendear(+L,+S,?R) ← R es la lista resultante de unir las listas L y S
Ej.: appendear([1,2,3],[4,5,6],[1,2,3,4,5,6]).
*/

appendear([], S, S).
appendear([X|L], S, [X|R]) :- appendear(L,S,R).

/*
pares_acc(+L1,+Acc, ?L2) ← L2 es la lista que contiene los elementos pares de L1, usando Acumulador Ac (inicializado lista vacia).
Ej: pares_acc([1,2,3,4,5],[],[2,4]).
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
/*
pares(+L1,?L2) ← L2 es la lista que contiene los elementos pares de L1.
Ej: pares([1,2,3,4,5],[2,4]).
*/
pares(L1, L2):-
    pares_acc(L1, [], L2).

/*
pares_impares_acc(+L1,+Ac1, +Ac2, ?L2,?L3) ← L2 es una lista con los valores pares de la lista L1,
L3 es una lista con los valores impares de la lista L1, usando Acumuladores Ac1 y Ac2 (inicializados en listas vacias).
Ej.: pares_impares_acc([4,5,3,1,2],[],[],[4,2],[5,3,1]).
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

/*
pares_impares(+L1,?L2,?L3) ← L2 es una lista con los valores pares de la lista L1,
L3 es una lista con los valores impares de la lista L1.
Ej.: pares_impares([4,5,3,1,2],[4,2],[5,3,1]).
*/
pares_impares(L1, L2, L3):-
    pares_impares_acc(L1, [], [], L2, L3).


/*
mas_chico_acc(+L,+Ac, ?N) ← N es el elemento mas chico de la lista L, usando Acumuladores Ac inicializado con el primer elemento de L. 
Ej.: mas_chico_acc([5,3,1,2],4,1).
*/

mas_chico_acc([],Acc,Acc).
mas_chico_acc([X|L],Acc,N):-
    X < Acc,
    mas_chico_acc(L,X,N).

mas_chico_acc([X|L],Acc,N):-
    X >= Acc,
    mas_chico_acc(L,Acc,N).

/*
mas_chico(+L, ?N) ← N es el elemento mas chico de la lista L
Ej.: mas_chico([4,5,3,1,2],1).
*/
mas_chico([X|L],N):-
    mas_chico_acc(L,X,N).



/*
ordenada(+L1,?L2) ← L2 contiene los elementos de L1 ordenados de menor a mayor,
utilizando el algoritmo de ordenación por selección. Las listas contienen valores enteros y no
hay elementos repetidos.
Ej.: ordenada([4,5,3,1,2],[1,2,3,4,5]).
*/
ordenada([X],[X]).
ordenada(L1,[X|L2]):-
    mas_chico(L1,X),
    elegir_primero(X,L1,L1next),
    ordenada(L1next,L2).



% ############################################################################
%                         PARTE 2: Palabras Cruzadas
% ############################################################################

/*
lista(+N,-L) ← L es una lista de tamaño N que en sus celdas contiene variables.
?- lista(4,L).
*/
lista(0,[]).
lista(N,[_|L]):-
    N \= 0,
    N1 is N - 1,
    lista(N1,L).

/*
matriz_aux(+N,-M,+N) ←M es una matriz de tamaño N X N que en sus celdas contiene variables,
de modo que representa un tablero vacío. La matriz está representada como lista de listas.
?- matriz_aux(4,M,4).
*/
matriz_aux(0, [], _).
matriz_aux(N, [L|M], NBase):-
    lista(NBase, L),
    N \= 0,
    N1 is N - 1,
    matriz_aux(N1, M, NBase).

/*
matrizN(+N,-M) ← M es una matriz de tamaño N X N que en sus celdas contiene variables,
de modo que representa un tablero vacío. La matriz está representada como lista de listas.
?- matriz(4,M).
*/
matrizN(N, M):-
    matriz_aux(N, M, N).


% columna(+M,?C,?R) C es la primera columna de M en forma de lista, R es M sin la primera columna
columna([],[],[]).
columna([[X|V]|M],[X|C],[V|R]):- columna(M,C,R).

/* 
traspuesta(?M,?MT) ← MT es la traspuesta de la matriz M.
?- traspuesta([[A,B],[C,D]],MT).
MT = [[A,C],[B,D]]
*/
traspuesta([],[]).
traspuesta([[]|X],[]):- traspuesta(X,[]).
traspuesta(M,[C|L]) :- columna(M,C,R), traspuesta(R,L).



% ==============================================================
%                         PARTE 2.1
% ==============================================================

/*
cruzadas1(+N,?T) ← T es un tablero válido de tamaño N X N de palabras cruzadas, es
decir, todas las filas y todas las columnas contienen letras que forman palabras de largo N
pertenecientes al diccionario.
?- cruzadas1(3,T).
T = [[a,l,a],[c,a,l],[a,s,a]]
*/
generate([]).
generate([F|M]):-
    palabra(F),
    generate(M).

cruzadas1(N,T):-
    matrizN(N,M),
    generate(M),
    traspuesta(M,T),
    generate(T).


% ==============================================================
%                         PARTE 2.2
% ==============================================================

/*
intercaladas(+M1,+M2,?I) ← I es una lista que contiene las filas de M y MT intercaladas.
M y MT son de igual tamaño.
?- intercaladas([[1,2,3],[4,5,6],[7,8,9]], [[1,4,7],[2,5,8],[3,6,9]], I).
I = [[1,2,3],[1,4,7],[4,5,6],[2,5,8],[7,8,9],[3,6,9]]
*/

intercaladas([],[],[]).
intercaladas([M1|R1],[M2|R2],[M1,M2|I]):-
    intercaladas(R1,R2,I).

/*
cruzadas2(+N,?T) ← T es un tablero válido de tamaño N X N de palabras cruzadas, es
decir, todas las filas y todas las columnas contienen letras que forman palabras de largo N
pertenecientes al diccionario.
*/
cruzadas2(N,T):-
    matrizN(N,M),
    traspuesta(M,T),
    intercaladas(M,T,I),
    generate(I).


% ==============================================================
%                         PARTE 2.3
% ==============================================================


% ================================================================
% Comparativa de rendimiento - cruzadas1/2
% ------------------------------------------------
% Diccionario chico (time(findall(T, cruzadas1(N,T), _))):
% ------------------------------------------------
% |   N   | Inferences |   CPU Time   | Real Time  |     CPU %      |       Lips       |
% |-------|------------|--------------|------------|----------------|------------------|
% |   2   |     833    |   0.000 s    |  0.000 s   |     0%         |   Infinite       |
% |   3   |   23,720   |   0.000 s    |  0.002 s   |     0%         |   Infinite       |
% |   4   |  332,438   |   0.031 s    |  0.032 s   |    97%         |   10,638,016     |

% ------------------------------------------------
% Diccionario grande time(once(cruzadas1(N, T))):
% ------------------------------------------------
% |   N   | Inferences |   CPU Time   | Real Time  |     CPU %      |       Lips       |
% |-------|------------|--------------|------------|----------------|------------------|
% |   2   |     278    |   0.219 s    |  0.223 s   |    98%         |   1,271          |
% |   3   |  486,599   |  21.609 s    | 21.636 s   |   100%         |   22,518         |
% |   4   |     —      |     —        |    —       |      —         |   No terminó     |

% ================================================================
% Comparativa de rendimiento - cruzadas2/2
% ------------------------------------------------
% Diccionario chico (time(findall(T, cruzadas2(N,T), _))):
% ------------------------------------------------
% |   N   | Inferences |   CPU Time   | Real Time  |     CPU %      |       Lips       |
% |-------|------------|--------------|------------|----------------|------------------|
% |   2   |     147    |   0.000 s    |  0.000 s   |     0%         |   Infinite       |
% |   3   |     602    |   0.000 s    |  0.000 s   |     0%         |   Infinite       |
% |   4   |     339    |   0.000 s    |  0.000 s   |     0%         |   Infinite       |

% ------------------------------------------------
% Diccionario grande (time(once(cruzadas2(N, T))):
% ------------------------------------------------
% |   N   | Inferences |   CPU Time   | Real Time  |     CPU %      |       Lips       |
% |-------|------------|--------------|------------|----------------|------------------|
% |   2   |      39    |   0.000 s    |  0.000 s   |     0%         |   Infinite       |
% |   3   |      65    |   0.016 s    |  0.001 s   |   1489%        |   4,160          |
% |   4   |      97    |   0.094 s    |  0.080 s   |   118%         |   1,035          |
% |   5   |   1,955    |   0.656 s    |  0.662 s   |    99%         |   2,979          |
% |   6   | 3,408,557  | 1145.219 s   | 1147.028 s |   100%         |   2,976          |
% ================================================================


% ==============================================================
%                         CONCLUSIÓN
% ==============================================================

/*
Generar primero todas las filas sin verificar las columnas hace que se recorran
muchas combinaciones que luego no sirven, porque las columnas pueden no formar
palabras válidas. En cambio, si se generan filas y columnas al mismo tiempo,
es posible detectar más rápido si una letra no va a funcionar en alguna de las
dos direcciones. Esto permite podar el árbol de búsqueda antes, ahorrando
trabajo y mejorando mucho la eficiencia del programa.
*/