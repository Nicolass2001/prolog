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
 

