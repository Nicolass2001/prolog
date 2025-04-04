/* Ejercicio 1 -----------------------------------------------------------------
Se busca colorear un mapa, de forma que no haya dos países vecinos con los mismos colores.
El mapa se representa por una lista de regiones de la forma: region(Nombre, Color, ColoresVecinos)
En la figura: [region(a, A, [B,C,D]), region(b, B, [A,C,E]), ...]
Defina el siguiente predicado:
colorear(Mapa, Colores) 
*/
% Generador: Asigna colores a cada región del mapa
asignar_colores([], _). % Caso base: lista vacía, todo coloreado.
asignar_colores([region(_, Color, _) | Resto], Colores) :-
    member(Color, Colores), % Genera un color para la región
    asignar_colores(Resto, Colores). % Repite para el resto del mapa

% Verificador: Confirma que no haya vecinos con el mismo color
verificar_vecinos([]).
verificar_vecinos([region(_, Color, Vecinos) | Resto]) :-
    \+ member(Color, Vecinos), % Verifica la región actual
    verificar_vecinos(Resto). % Repite para el resto del mapa

colorear(Mapa,Colores):-
    %Generate
    asignar_colores(Mapa,Colores),
    %Check
    verificar_vecinos(Mapa).

/* Ejercicio 2 ----------------------------------------------------------------
El problema del «ataque de las k reinas» consiste en
distribuir k reinas en un tablero de n por n, de forma que
toda casilla del tablero quede atacada por una reina, y
ninguna reina sea atacada por otra.
Defina el siguiente predicado:
kreinas(K,N,Reinas) 
Reinas es una solución al problema del ataque de las K
reinas en un tablero de tamaño N por N.
*/

lista(0,[]).
lista(N,[N|L]):-
    N1 is N - 1,
    lista(N1,L).

kreinas_generate(0,_,_,[]).
kreinas_generate(K,X,Y,[R|Reinas]):-
    K1 is K - 1,
    select(SelectedX,X,X1),
    select(SelectedY,Y,Y1),
    R = [SelectedX,SelectedY],
    kreinas_generate(K1,X1,Y1,Reinas).

not_diagonal([Ax,Ay],[Bx,By]):-
    Y is abs(Bx - Ax),
    X is abs(By - Ay),
    Y \= X.

check_diagonal(_,[]).
check_diagonal(R,[R1|Reinas]):-
    not_diagonal(R,R1),
    check_diagonal(R, Reinas).

kreinas_check([_]).
kreinas_check([R|Reinas]):-
    check_diagonal(R, Reinas),
    kreinas_check(Reinas).

kreinas(K,N,Reinas):-
    % Generate
    length(X, N),
    lista(N, X),
    length(Y, N),
    lista(N, Y),
    kreinas_generate(K,X,Y,Reinas),
    % Check
    kreinas_check(Reinas).

/* Otra solución
% sublista(L,S) ← La lista S contiene elementos consecutivos de la lista L.
sublista(L,S):- append(P,_,L), append(_,S,P).


% Genera una lista con N elementos iguales a Elem
generar_fila(0, _, []).
generar_fila(N, Elem, [Elem | Resto]) :-
    N > 0,
    N1 is N - 1,
    generar_fila(N1, Elem, Resto).

% Genera un tablero NxN con el valor inicial Elem en todas las casillas
generar_tablero(0,_, _, []).
generar_tablero(N, F, Elem, [Fila | Resto]) :-
    N > 0,
    generar_fila(F, Elem, Fila), 
    N1 is N - 1,
    generar_tablero(N1, F, Elem, Resto).


asignar_reina(1,[_|Fila],[1|Fila]).
asignar_reina(IndicePosicion,[X|Fila],[X|FilaReina]):-
        IndiceNuevo is IndicePosicion - 1,
        asignar_reina(IndiceNuevo,Fila,FilaReina).

asignar_reinas(_, [], [], []).

asignar_reinas(IndiceFila,[IndiceFila|Resto],[Fila|RestoTablero], [FilaReina|TableroFinal]):-
    length(Fila, LargoFila),
    numlist(1,LargoFila , Posicionesfila),
    member(IndicePosicion, Posicionesfila),
    asignar_reina(IndicePosicion, Fila, FilaReina),
    FilaSiguiente is IndiceFila + 1,
    asignar_reinas(FilaSiguiente,Resto, RestoTablero, TableroFinal).
asignar_reinas(FilaAcual,FilasCandidatas,[Fila|RestoTablero], [Fila|TableroFinal]):-
    FilaSiguiente is FilaAcual + 1,
    asignar_reinas(FilaSiguiente,FilasCandidatas, RestoTablero, TableroFinal).


%comprobar_reinas([]).
comprobar_reinas([[]|_]).

comprobar_reinas(Reinas):-
    columna(Reinas,ReinaColuma,ReinaColumas),
   % write(ReinaColuma), nl,
    selectchk(1,ReinaColuma,Resto),
  %  write(Resto), nl,
    \+ member(1, Resto),
 
    comprobar_reinas(ReinaColumas).

comprobar_reinas(Reinas):-
    columna(Reinas,ReinaColuma,ReinaColumas),
     % write(ReinaColuma), nl,
    \+ member(1, ReinaColuma),
    comprobar_reinas(ReinaColumas).


kreinas(K,N,Reinas):-
%Generate

generar_tablero(N, N, 0, TableroInicial),
numilst(1, N, TodasFilas),
sublista(TodasFilas, FilasCandidatas),
length(FilasCandidatas, K),  % Filtrar solo combinaciones de tamaño K
asignar_reinas(1,FilasCandidatas, TableroInicial, Reinas),
 
%Check
comprobar_reinas(Reinas).
%Falta la diagonal
*/

/* Ejercicio 3 ----------------------------------------------------------------
Al principio hay tres peones blancos y tres negros,
alineados y separados por una casilla vacía.
En cada jugada se realiza una de las cuatro siguientes
modificaciones:
 Movimiento a la izquierda de un peón negro
 Salto a la derecha de un peón blanco sobre uno negro
 Movimiento a la derecha de un peón blanco
 Salto a la izquierda de un peón negro sobre uno blanco
Defina el siguiente predicado:
peones(Movs) Movs es la lista de movimientos
necesarios para intercambiar los
peones blancos y negros.
%[B,B,B,,N,N,N] 
*/

hacer_movimiento([vacio,negro|Resto], m1, TableroSiguiente):- 
    TableroSiguiente = [negro,vacio|Resto].
hacer_movimiento([blanco,negro,vacio|Resto], m2, TableroSiguiente):- 
    TableroSiguiente = [vacio,negro,blanco|Resto].
hacer_movimiento([blanco,vacio|Resto], m3, TableroSiguiente):-
    TableroSiguiente = [vacio,blanco|Resto].
hacer_movimiento([vacio,blanco,negro|Resto], m4, TableroSiguiente):- 
    TableroSiguiente = [negro,blanco,vacio|Resto].
hacer_movimiento([X|RestoTablero], Movimiento, [X|TableroSiguiente]):-
    hacer_movimiento(RestoTablero, Movimiento, TableroSiguiente).

movimientos_candidatos(Tablero, Movimientos, TableroFinal, MovimientosFinal):-
    member(Movimiento, [m1,m2,m3,m4]),
    hacer_movimiento(Tablero, Movimiento,TableroSiguiente),
    movimientos_candidatos(TableroSiguiente, [Movimiento|Movimientos], TableroFinal, MovimientosFinal).

movimientos_candidatos(Tablero, Movimientos, TableroFinal, MovimientosFinal):-
    TableroFinal = Tablero,
    MovimientosFinal = Movimientos.

peones(Movs):-
%Generate
    Tablero = [blanco,blanco,blanco,vacio,negro,negro,negro],
    Movimientos = [],
    movimientos_candidatos(Tablero, Movimientos, TableroFinal, Movs),
%Check
    TableroFinal = [negro,negro,negro,vacio,blanco,blanco,blanco],
    write(Movs), nl.

/* Ejercicio 4 ----------------------------------------------------------------
Escriba un programa Prolog que resuelva el problema de las novias celosas. Este
problema consiste en tres parejas que deben cruzar un río de una orilla a otra en un
bote cuya capacidad es de dos personas. La solución consiste en encontrar una
secuencia de viajes de manera tal que en ningún momento un hombre se encuentre
en presencia de otras mujeres sin su novia. Tenga en cuenta que, en cada cruce,
cualquiera de los seis puede manejar el bote si se encuentra en su orilla.
*/
hombre(h1).
hombre(h2).
hombre(h3).
mujer(m1).
mujer(m2).
mujer(m3).
pareja(h1,m1).
pareja(h2,m2).
pareja(h3,m3).
grupo_invalido(G):-
    member(H,G),
    member(M,G),
    hombre(H),
    mujer(M),
    \+ pareja(H,M),
    pareja(H,M2),
    \+ member(M2,G).

generate_viajes([[],_,_],_,[]).
generate_viajes([OrillaIzquierda,OrillaDerecha,bote_izq],EstadoHastaAhora,[[X,Y]|Camino]):-
    \+ member([OrillaIzquierda,OrillaDerecha,bote_izq],EstadoHastaAhora),
    select(X,OrillaIzquierda,RestoOrillaIzquierdaAux),
    select(Y,RestoOrillaIzquierdaAux,RestoOrillaIzquierda),
    \+ grupo_invalido(RestoOrillaIzquierda),
    \+ grupo_invalido([X,Y]),
    append(OrillaDerecha,[X,Y],NewOrillaDerecha),
    \+ grupo_invalido(NewOrillaDerecha),
    generate_viajes([RestoOrillaIzquierda,NewOrillaDerecha,bote_der],[[OrillaIzquierda,OrillaDerecha,bote_izq]|EstadoHastaAhora],Camino).

generate_viajes([OrillaIzquierda,OrillaDerecha,bote_izq],EstadoHastaAhora,[[X]|Camino]):-
    \+ member([OrillaIzquierda,OrillaDerecha,bote_izq],EstadoHastaAhora),
    select(X,OrillaIzquierda,RestoOrillaIzquierda),
    \+ grupo_invalido(RestoOrillaIzquierda),
    append(OrillaDerecha,[X],NewOrillaDerecha),
    \+ grupo_invalido(NewOrillaDerecha),
    generate_viajes([RestoOrillaIzquierda,NewOrillaDerecha,bote_der],[[OrillaIzquierda,OrillaDerecha,bote_izq]|EstadoHastaAhora],Camino).

generate_viajes([OrillaIzquierda,OrillaDerecha,bote_der],EstadoHastaAhora,[[X,Y]|Camino]):-
    \+ member([OrillaIzquierda,OrillaDerecha,bote_der],EstadoHastaAhora),
    select(X,OrillaDerecha,RestoOrillaDerechaAux),
    select(Y,RestoOrillaDerechaAux,RestoOrillaDerecha),
    \+ grupo_invalido(RestoOrillaDerecha),
    \+ grupo_invalido([X,Y]),
    append(OrillaDerecha,[X,Y],NewOrillaIzquierda),
    \+ grupo_invalido(NewOrillaIzquierda),
    generate_viajes([NewOrillaIzquierda,RestoOrillaDerecha,bote_izq],[[OrillaIzquierda,OrillaDerecha,bote_der]|EstadoHastaAhora],Camino).
    
generate_viajes([OrillaIzquierda,OrillaDerecha,bote_der],EstadoHastaAhora,[[X]|Camino]):-
    \+ member([OrillaIzquierda,OrillaDerecha,bote_der],EstadoHastaAhora),
    select(X,OrillaDerecha,RestoOrillaDerecha),
    \+ grupo_invalido(RestoOrillaDerecha),
    append(OrillaDerecha,[X],NewOrillaIzquierda),
    \+ grupo_invalido(NewOrillaIzquierda),
    generate_viajes([NewOrillaIzquierda,RestoOrillaDerecha,bote_izq],[[OrillaIzquierda,OrillaDerecha,bote_der]|EstadoHastaAhora],Camino).

/*
generate_viajes([[],_,_],_,_,).
generate_viajes(Estado1,[SiguienteBarco|V]):-
    viaje(Estado1, Estado2)
    generate_viajes(Estado2).

generate_viajes([OrillaIzquierda,OrillaDerecha,bote_izq],[SiguienteBarco|V]):-
    select(X,OrillaIzquierda,RestoOrillaIzquierda),
    \+ grupo_invalido([X,Y]),
    generate_viajes([RestoOrillaIzquierda,NewOrillaDerecha,bote_der]).
generate_viajes([OrillaIzquierda,Barco,OrillaDerecha,bote_der],[SiguienteBarco|V],[SiguienteOrillaIzquierda,SiguienteBarco,SiguienteOrillaDerecha]):-
 */

viajes(V):-
    %Generate
    EstadoInicial = [[h1,m1,h2,m2,h3,m3],[],bote_izq],
    generate_viajes(EstadoInicial,[],V).
 
/* Ejercicio 5 ----------------------------------------------------------------
Este juego consiste en completar con cuadros negros una grilla de N X M casillas
respetando ciertas indicaciones asociadas a las filas y columnas. Cada una de estas
indicaciones consiste en una lista de números. Cada número representa el largo de
una secuencia de cuadros negros en la fila o columna a la que está asociada. En el
ejemplo que se da a continuación, la lista [3,2] asociada a la fila 3 indica que en esa
fila hay dos (y sólo 2) secuencias de cuadros negros: la primera de largo 3 y la
segunda de largo 2.
*/
% Dado una lista L devuelve la lista R donde R es L con los elementos de X en adelante
remove_before([X|L],X,L).
remove_before([Y|L],X,L1):-
    X \= Y,
    remove_before(L,X,L1).

% Dado N devuelve una lista L con los números del 1 a N
lista_aux(N,L):-
    length(LAux,N),
    lista(N,LAux),
    reverse(LAux,L).
/* generar_fila(Restricciones, Segmentos, L) -> Restricciones de la forma [1,3,4] y L es un array
de números de 1 a N segmentos es un array de duplas que indican donde empieza y termina el segmento */
generar_fila([],[],_).
generar_fila([Restriccion|Restricciones], [[Seg1,Seg2]|Segmentos], L):-
    member(Seg1,L),
    Seg2 is Seg1 + Restriccion,
    remove_before(L, Seg2, LNew),
    generar_fila(Restricciones, Segmentos, LNew).

% Función auxiliar para llamar generar_fila 
generar_fila_aux(Restricciones, S, N):-
    NAux is N + 1,
    lista_aux(NAux,L),
    generar_fila(Restricciones, S, L).

% Genera una lista L de largo N con los elementos E generar_lista(L, N, E)
generar_lista([],0,_).
generar_lista([E|L], N, E):-
    NNew is N - 1,
    generar_lista(L, NNew, E).

generar_lista_aux(L, N, E):-
    length(L, N),
    generar_lista(L, N, E).

% Generar fila matriz en función de generar_fila
generar_fila_matriz([], Fila, LargoFila, PrimerElemento):-
    LargoVacios is LargoFila - PrimerElemento + 1,
    generar_lista_aux(Fila, LargoVacios, v).
generar_fila_matriz([[Seg1,Seg2]|Segmentos], Fila, LargoFila, PrimerElemento):-
    LargoVacios is Seg1 - PrimerElemento,
    generar_lista_aux(Vacios, LargoVacios, v),
    LargoMarcados is Seg2 - Seg1,
    generar_lista_aux(Marcados, LargoMarcados, m),
    append(Vacios, Marcados, FilaPaso),
    generar_fila_matriz(Segmentos, FilaAnterior, LargoFila, Seg2),
    append(FilaPaso, FilaAnterior, Fila).

generar_fila_matriz_aux(Restricciones, LargoFila, FilasMatriz):-
    generar_fila_aux(Restricciones, Segmentos, LargoFila),
    generar_fila_matriz(Segmentos, FilasMatriz, LargoFila, 1).

% Generar matriz de en función de las restricciones de filas
generar_matriz([], [], _).
generar_matriz([RestriccionesFila|RestTotales], [Fila|Matriz], LargoFila):-
    generar_fila_matriz_aux(RestriccionesFila, LargoFila, Fila),
    generar_matriz(RestTotales, Matriz, LargoFila).

generar_matriz_test(M):-
    RestriccionesFilas = [[1],[3],[1,2],[4]],
    generar_matriz(RestriccionesFilas, M, 4),
    write(M), nl.

% Check -----------------------------------------------------------------------

contar_m([], 0, []).
contar_m([X|L], 0, L):-
    X \= m.
contar_m([m|L], N, R):-
    contar_m(L, NSig, R),
    N is NSig + 1.

sacar_v([],[]).
sacar_v([X|L],[X|L]):-
    X \= v.
sacar_v([v|L],L1):-
    sacar_v(L,L1).

contar_m_aux(L,N,R):-
    sacar_v(L,L1),
    contar_m(L1,N,L2),
    sacar_v(L2,R).

contar_m_aux_test(N,R):-
    L = [v,m,m,m,v],
    contar_m_aux(L,N,R).

/* Función recibe [v,v,m,m,v,m] y devuelve una lista de largo 
(cantidad de secciones y el valor es el largo de la sección) */
obtener_restricciones([],[]).
obtener_restricciones(Columna, [N|Restricciones]):-
    Columna \= [],
    contar_m_aux(Columna,N,NextColumna),
    obtener_restricciones(NextColumna, Restricciones).

obtener_restricciones_test(R):-
    Col = [v,m,m,v,v],
    obtener_restricciones(Col,R).

% Dada una matriz devuelve la primer columna
columna([],[],[]).
columna([[X|V]|M],[X|C],[V|R]):-
    columna(M,C,R).

obtener_restricciones_columnas([[]|_],[]).
obtener_restricciones_columnas(Matriz,[Rest|Restricciones]):-
    columna(Matriz,Col,RestMat),
    obtener_restricciones(Col,Rest),
    obtener_restricciones_columnas(RestMat,Restricciones).

obtener_restricciones_columnas_test(R):-
    M = [[v,m,m,m],[m,m,v,m],[v,m,m,m],[v,v,m,m]],
    obtener_restricciones_columnas(M,R).

puzle([RestriccionesFilas, RestriccionesColumnas], Matriz):-
    % Generate
    length(RestriccionesColumnas, LargoFila),
    generar_matriz(RestriccionesFilas, Matriz, LargoFila),
    %Check
    obtener_restricciones_columnas(Matriz, RestriccionesColumnasGeneradas),
    RestriccionesColumnas = RestriccionesColumnasGeneradas.


puzle_test(M):-
    R = [[[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]],
    [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]],
    puzle(R,M),
    write(M), nl.

/*
puzle_test(M):-
    R = [[[3],[2,1],[3],[2]],
    [[1],[3],[1,2],[4]]],
    puzle(R,M),
    write(M), nl.
*/
