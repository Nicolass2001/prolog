:- module(ceritolog,
[
tablero/2, % tablero(+N,?Tablero)
% Devuelve un tablero de tamaño N vacío, o sea una matriz que representa un
% tablero vacío de juego como la descrita en la letra del laboratorio.

fin_del_juego/4, % fin_del_juego(+Tablero,?P1,?P2,?Ganador)
% Dado un tablero, el predicado es verdadero si el tablero representa un juego
% finalizado, y devuelve % la cantidad de puntos del jugador 1 en P1, la
% cantidad de puntos del jugador 2 en P2, y un string % que indica si alguno
% ganó, en el formato: “Gana el jugador 1”, “Gana el jugador 2”, o “Empate”.
% En caso de que no sea el fin del juego, el predicado falla.
 
jugada_humano/8, % jugada_humano(+Tablero,+Turno,+F,+C,+D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y la línea elegida por el
% jugador humano con las variables F-C-D, y devuelve: el tablero modificado con
% la línea marcada (y celdas marcadas en caso de que sea necesario), de quién es
% el siguiente turno (Turno2), y una lista de celdas que se capturaron con esta
% acción en formato [Fila,Columna]. Por ejemplo: [[1,2],[1,3]]

jugada_maquina/9, % jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y el Nivel de minimax,
% debe elegir una jugada a realizar por el jugador controlado por la computadora.
% El predicado devuelve: el tablero modificado luego de la jugada, de quién es
% el siguiente turno (Turno2), y una lista de celdas que se cerraron con esta
% acción en formato [Fila,Columna], de la misma forma que en el predicado anterior.

sugerencia_jugada/6 % sugerencia_jugada(+Tablero,+Turno,+Nivel,?F,?C,?D)
% Utiliza la estrategia de minimax para calcular una buena jugada para sugerirle
% a un jugador humano.
]).

% tablero(+N,?Tablero)
% Devuelve un tablero de tamaño N vacío, o sea una matriz que representa un
% tablero vacío de juego como la descrita en la letra del laboratorio.

fila(0, []).
fila(N, [c(0,0,0)|R]) :-
    N > 0,
    N1 is N - 1, 
    fila(N1, R).

filas(_, 0, []).
filas(N, C, [F|R]) :-
    C > 0,
    fila(N, FLista),
    F =.. [f|FLista],
    C1 is C - 1,
    filas(N, C1, R).

tablero(N, T) :-
    filas(N, N, TLista),
    T =.. [m|TLista].

% fin_del_juego(+Tablero,?P1,?P2,?Ganador)
% Dado un tablero, el predicado es verdadero si el tablero representa un juego
% finalizado, y devuelve % la cantidad de puntos del jugador 1 en P1, la
% cantidad de puntos del jugador 2 en P2, y un string % que indica si alguno
% ganó, en el formato: “Gana el jugador 1”, “Gana el jugador 2”, o “Empate”.
% En caso de que no sea el fin del juego, el predicado falla.

recorrer_fila([_],0,0).
recorrer_fila([c(_,_,2)|C],P1,P2new):-
    recorrer_fila(C,P1,P2),
    P2new is P2 + 1.
recorrer_fila([c(_,_,1)|C],P1new,P2):-
    recorrer_fila(C,P1,P2),
    P1new is P1 + 1.

recorrer_fila_aux(C,P1,P2):-
    C =.. [_|Args],
    recorrer_fila(Args,P1,P2).

recorrer_tablero([_],0,0).
recorrer_tablero([C|T],P1,P2):-
    recorrer_fila_aux(C,PC1,PC2),
    recorrer_tablero(T,PT1,PT2),
    P1 is PC1 + PT1,
    P2 is PC2 + PT2.

recorrer_tablero_aux(T,P1,P2):-
    T =.. [_|Args],
    recorrer_tablero(Args,P1,P2).

mensaje_ganador(P1,P2,'Gana el jugador 1'):- P1 > P2.
mensaje_ganador(P1,P2,'Gana el jugador 2'):- P2 > P1.
mensaje_ganador(P1,P2,'Empate'):- P2 =:= P1.

fin_del_juego(T,P1,P2,G):-
    recorrer_tablero_aux(T,P1,P2),
    mensaje_ganador(P1,P2,G).

% jugada_humano(+Tablero,+Turno,+F,+C,+D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y la línea elegida por el
% jugador humano con las variables F-C-D, y devuelve: el tablero modificado con
% la línea marcada (y celdas marcadas en caso de que sea necesario), de quién es
% el siguiente turno (Turno2), y una lista de celdas que se capturaron con esta
% acción en formato [Fila,Columna]. Por ejemplo: [[1,2],[1,3]]

poner_pared_en_celda(Celda,h):-
    setarg(1,Celda,1).

poner_pared_en_celda(Celda,v):-
    setarg(2,Celda,1).

poner_pared_en_tablero(Tablero,F,C,D):-
    Tablero =.. [_|TFilas],
    nth1(F,TFilas,Fila),
    Fila =.. [_|FCeldas],
    nth1(C,FCeldas,Celda),
    poner_pared_en_celda(Celda,D).

chequear_celda(Tablero,Turno,F,C,CeldaDevuelta):-
    %Chequear celda F, C
    Tablero =.. [_|TFilas],
    nth1(F,TFilas,Fila),
    Fila =.. [_|FCeldas],
    nth1(C,FCeldas,Celda),
    Celda =.. [_,1,1,_],
    %Chequear celda F, C + 1
    CMasUno is C + 1,
    nth1(F,TFilas,FilaSegundoCaso),
    FilaSegundoCaso =.. [_|FCeldasSegundoCaso],
    nth1(CMasUno,FCeldasSegundoCaso,CeldaSegundoCaso),
    CeldaSegundoCaso =.. [_,_,1,_],
    %Chequear celda F + 1, C
    FMasUno is F + 1,
    nth1(FMasUno,TFilas,FilaTercerCaso),
    FilaTercerCaso =.. [_|FCeldasTercerCaso],
    nth1(C,FCeldasTercerCaso,CeldaTercerCaso),
    CeldaTercerCaso =.. [_,1,_,_],
    %Pintar celda
    setarg(3,Celda,Turno),
    CeldaDevuelta = [[F,C]],
    !.
chequear_celda(_,_,_,_,[]).

poner_celda_en_tablero(Tablero,Turno,F,C,h,Celdas):-
    chequear_celda(Tablero,Turno,F,C,Celda1),
    FMenosUno is F - 1,
    chequear_celda(Tablero,Turno,FMenosUno,C,Celda2),
    append(Celda1,Celda2,Celdas).

poner_celda_en_tablero(Tablero,Turno,F,C,v,Celdas):-
    chequear_celda(Tablero,Turno,F,C,Celda1),
    CMenosUno is C - 1,
    chequear_celda(Tablero,Turno,F,CMenosUno,Celda2),
    append(Celda1,Celda2,Celdas).

siguiente_turno([],1,2):- !.
siguiente_turno([],2,1):- !.
siguiente_turno(_,1,1).
siguiente_turno(_,2,2).

celda(M,I,J,V):-
    M =.. [_|Filas],
    nth1(I,Filas,Fila),
    Fila =.. [_|Valores],
    nth1(J,Valores,V).

jugada_valida(Tablero,F,C,h):-
    celda(Tablero,F,C,V),
    V =.. [_,0,_,_].
jugada_valida(Tablero,F,C,v):-
    celda(Tablero,F,C,V),
    V =.. [_,_,0,_].

jugada_humano(Tablero,Turno,F,C,D,Tablero2,Turno2,Celdas):-
    jugada_valida(Tablero,F,C,D),
    poner_pared_en_tablero(Tablero,F,C,D),
    poner_celda_en_tablero(Tablero,Turno,F,C,D,Celdas),
    siguiente_turno(Celdas,Turno,Turno2),
    Tablero2 = Tablero.

% jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y el Nivel de minimax,
% debe elegir una jugada a realizar por el jugador controlado por la computadora.
% El predicado devuelve: el tablero modificado luego de la jugada, de quién es
% el siguiente turno (Turno2), y una lista de celdas que se cerraron con esta
% acción en formato [Fila,Columna], de la misma forma que en el predicado anterior.

jugada_maquina(Tablero,Turno,Nivel,F,C,D,Tablero2,Turno2,Celdas):-
    copy_term(Tablero,Tablero3),
    minimax(Tablero3,Turno,Nivel,F,C,D,maxing,_),
    write(Tablero),
    jugada_humano(Tablero,Turno,F,C,D,Tablero2,Turno2,Celdas).

calcular_value(_,P1,P2,Value):-
    P1 = P2,
    Value = 0, !.
calcular_value(Turno,P1,P2,Value):-
    P1 > P2,
    Turno = 1,
    Value = 100, !.
calcular_value(Turno,P1,P2,Value):-
    P2 > P1,
    Turno = 2,
    Value = 100, !.
calcular_value(Turno,P1,P2,Value):-
    P1 > P2,
    Turno = 2,
    Value = -100, !.
calcular_value(Turno,P1,P2,Value):-
    P2 > P1,
    Turno = 1,
    Value = -100, !.

next_jugador(Tablero,Turno,F,C,D,Tablero2,Turno2):-
    jugada_humano(Tablero,Turno,F,C,D,Tablero2,Turno2,_),
    Turno \= Turno2, !.
next_jugador(Tablero,Turno,F,C,D,TableroFinal,TurnoFinal):-
    jugada_humano(Tablero,Turno,F,C,D,Tablero2,Turno2,_),
    Turno = Turno2,
    next_jugador(Tablero2,Turno2,_,_,_,TableroFinal,TurnoFinal).

% Caso base: el máximo de una lista de un solo elemento es ese elemento.
max_list([X], X).

% Caso recursivo: compara el primer elemento con el máximo del resto de la lista.
max_list([X|Rest], Max) :-
    max_list(Rest, MaxRest),
    Max is max(X, MaxRest).

% Caso base: el mínimo de una lista de un solo elemento es ese elemento.
min_list([X], X).

% Caso recursivo: compara el primer elemento con el mínimo del resto de la lista.
min_list([X|Rest], Min) :-
    min_list(Rest, MinRest),
    Min is min(X, MinRest).
    
otro_turno(1,2).
otro_turno(2,1).

heuristic(_,0).

minimax(Tablero,Turno,_,_,_,_,_,Value):-
    fin_del_juego(Tablero,P1,P2,_),
    calcular_value(Turno,P1,P2,Value), !.

minimax(Tablero,_,0,_,_,_,_,Value):-
    heuristic(Tablero,Value), !.

minimax(Tablero,Turno,Nivel,F,C,D,maxing,Value):-
    next_jugador(Tablero,Turno,F,C,D,Tablero2,_),
    NivelNuevo is Nivel - 1,
    findall(ValueMin, minimax(Tablero2,Turno,NivelNuevo,_,_,_,mining,ValueMin), L),
    max_list(L,Value).

minimax(Tablero,Turno,Nivel,F,C,D,mining,Value):-
    otro_turno(Turno, OtroTurno),
    next_jugador(Tablero,OtroTurno,F,C,D,Tablero2,_),
    NivelNuevo is Nivel - 1,
    findall(ValueMin, minimax(Tablero2,Turno,NivelNuevo,_,_,_,maxing,ValueMin), L),
    min_list(L,Value).

/*
function minimax(node, depth, is_maximizing)
if node is a terminal node or depth = 0
 return the heuristic value of node
else if is_maximizing
 max_value := -∞
 foreach child of node
 max_value := max(max_value, minimax(child,depth-1,false))
 return max_value
else
 min_value := ∞
 foreach child of node
 min_value := min(min_value, minimax(child,depth-1,true))
 return min_value
*/


 

sugerencia_jugada(_,_,_,_,_,_):-fail.
