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