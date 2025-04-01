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