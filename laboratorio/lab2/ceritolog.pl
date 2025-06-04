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

% funciones auxiliares

% Devuelve en V la celda (I,J) de la matriz M
celda(M,I,J,V):-
    M =.. [_|Filas],
    nth1(I,Filas,Fila),
    Fila =.. [_|Valores],
    nth1(J,Valores,V).

% Devuelve en T2 una copia del tablero T1 que no comparte memoria
copia_tablero(T1,T2):-
    T1 =.. [_|Filas],
    copia_filas(Filas, NewFilas),
    T2 =.. [m|NewFilas].

copia_filas([],[]).
copia_filas([Fila|F1],[NewFila|F2]):-
    Fila =.. [_|Celdas],
    copia_celdas(Celdas,NewCeldas),
    NewFila =.. [f|NewCeldas],
    copia_filas(F1,F2).

copia_celdas([],[]).
copia_celdas([c(H,V,P)|C1],[c(H,V,P)|C2]):-
    copia_celdas(C1,C2).

% Devuelve en X el valor máximo de la lista L
max_list([X], X).
max_list([X|Rest], Max) :-
    max_list(Rest, MaxRest),
    Max is max(X, MaxRest).

% Devuelve en X el valor mínimo de la lista L
min_list([X], X).
min_list([X|Rest], Min) :-
    min_list(Rest, MinRest),
    Min is min(X, MinRest).

% Utiliza max_list o min_list dependiendo de si esta maximizando o minimizando
minimax_list(L,V,maxing):-
    max_list(L,V).
minimax_list(L,V,mining):-
    min_list(L,V).

% Devuelve el otro jugador
otro_turno(1,2).
otro_turno(2,1).

% Cuenta los puntos en el Tablero para P1 (Jugador 1) y P2 (Jugador 2)
contar_puntos(Tablero,P1,P2):-
    Tablero =.. [_|Filas],
    contar_puntos_filas(Filas,P1,P2).

contar_puntos_filas([],0,0):- !.
contar_puntos_filas([Fila|Filas],P1,P2):-
    Fila =.. [_|Celdas],
    contar_puntos_fila(Celdas,P1Fila,P2Fila),
    contar_puntos_filas(Filas,P1Filas,P2Filas),
    P1 is P1Filas + P1Fila,
    P2 is P2Filas + P2Fila.

contar_puntos_fila([],0,0):- !.
contar_puntos_fila([c(_,_,0)|Fila],P1,P2):-
    contar_puntos_fila(Fila,P1,P2), !.
contar_puntos_fila([c(_,_,1)|Fila],P1,P2):-
    contar_puntos_fila(Fila,P1Fila,P2),
    P1 is P1Fila + 1, !.
contar_puntos_fila([c(_,_,2)|Fila],P1,P2):-
    contar_puntos_fila(Fila,P1,P2Fila),
    P2 is P2Fila + 1.

% tablero(+N,?Tablero)
% Devuelve un tablero de tamaño N vacío, o sea una matriz que representa un
% tablero vacío de juego como la descrita en la letra del laboratorio.

fila(1, [c(1,Y,Z)], c(_,Y,Z)):- !.
fila(N, [V|R], V) :-
    N1 is N - 1, 
    fila(N1, R, V).

filas(N, 1, [F]):- 
    fila(N, FLista, c(0,1,0)),
    F =.. [f|FLista], !.
filas(N, C, [F|R]) :-
    fila(N, FLista, c(0,0,0)),
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

fin_del_juego_fila([_],0,0).
fin_del_juego_fila([c(_,_,2)|C],P1,P2new):-
    fin_del_juego_fila(C,P1,P2),
    P2new is P2 + 1.
fin_del_juego_fila([c(_,_,1)|C],P1new,P2):-
    fin_del_juego_fila(C,P1,P2),
    P1new is P1 + 1.

fin_del_juego_tablero([_],0,0).
fin_del_juego_tablero([C|T],P1,P2):-
    C =.. [_|Celdas],
    fin_del_juego_fila(Celdas,PC1,PC2),
    fin_del_juego_tablero(T,PT1,PT2),
    P1 is PC1 + PT1,
    P2 is PC2 + PT2.

mensaje_ganador(P1,P2,'Gana el jugador 1'):- P1 > P2.
mensaje_ganador(P1,P2,'Gana el jugador 2'):- P2 > P1.
mensaje_ganador(P1,P2,'Empate'):- P2 =:= P1.

fin_del_juego(T,P1,P2,G):-
    T =.. [_|Filas],
    fin_del_juego_tablero(Filas,P1,P2),
    mensaje_ganador(P1,P2,G).

% jugada_humano(+Tablero,+Turno,+F,+C,+D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y la línea elegida por el
% jugador humano con las variables F-C-D, y devuelve: el tablero modificado con
% la línea marcada (y celdas marcadas en caso de que sea necesario), de quién es
% el siguiente turno (Turno2), y una lista de celdas que se capturaron con esta
% acción en formato [Fila,Columna]. Por ejemplo: [[1,2],[1,3]]

jugada_valida(Tablero,F,C,h):-
    celda(Tablero,F,C,V),
    V =.. [_,0,_,_].
jugada_valida(Tablero,F,C,v):-
    celda(Tablero,F,C,V),
    V =.. [_,_,0,_].

poner_pared_en_tablero(Tablero,F,C,v):-
    celda(Tablero,F,C,Celda),
    setarg(2,Celda,1).
poner_pared_en_tablero(Tablero,F,C,h):-
    celda(Tablero,F,C,Celda),
    setarg(1,Celda,1).

chequear_celda(Tablero,Turno,F,C,CeldaDevuelta):-
    %Chequear celda F, C
    celda(Tablero,F,C,Celda),
    Celda =.. [_,1,1,_],
    %Chequear celda F, C + 1
    CMasUno is C + 1,
    celda(Tablero,F,CMasUno,CeldaSegundoCaso),
    CeldaSegundoCaso =.. [_,_,1,_],
    %Chequear celda F + 1, C
    FMasUno is F + 1,
    celda(Tablero,FMasUno,C,CeldaTercerCaso),
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

heuristic(Tablero,1,Value):-
    contar_puntos(Tablero,P1,P2),
    Value is P1 - P2.
heuristic(Tablero,2,Value):-
    contar_puntos(Tablero,P1,P2),
    Value is P2 - P1.

% Caso tablero final
minimax(Value,TurnoInicial,_,_,Tablero,_):-
    fin_del_juego(Tablero,P1,P2,_),
    calcular_value(TurnoInicial,P1,P2,Value), !.
% Caso nivel 0
minimax(Value,TurnoInicial,0,_,Tablero,_):-
    heuristic(Tablero,TurnoInicial,Value), !.
% Caso recursivo
minimax(Value,TurnoInicial,Nivel,Maxing,Tablero,Turno):-
    NivelNuevo is Nivel - 1,
    findall(ValueAux,(
        jugada_humano(Tablero,Turno,_,_,_,Tablero3,Turno3,_),
        ( TurnoInicial = Turno3 ->
            minimax(ValueAux,TurnoInicial,NivelNuevo,maxing,Tablero3,Turno3),
        ;
            minimax(ValueAux,TurnoInicial,NivelNuevo,mining,Tablero3,Turno3),
        )
    ),L),
    minimax_list(L,Value,Maxing).

max_list_aux([X], X) :- !.
max_list_aux([X|Rest], Max) :-
    max_list_aux(Rest, TempMax),
    X = [VX|_],
    TempMax = [VT|_],
    ( VX >= VT -> Max = X ; Max = TempMax ).

minimax_inicial(Tablero,Turno,Nivel,F,C,D):-
    NivelNuevo is Nivel - 1,
    findall([Value,FAux,CAux,DAux],(
        jugada_humano(Tablero,Turno,FAux,CAux,DAux,Tablero2,Turno2,_),
        ( Nivel = 0 -> 
            Value = 0
        ;( Turno = Turno2 ->
            minimax(Value,Turno,NivelNuevo,maxing,Tablero2,Turno2)
        ;
            minimax(Value,Turno,NivelNuevo,mining,Tablero2,Turno2)
        ))
    ),L),
    random_permutation(L,L2),
    max_list_aux(L2,V),
    V = [_,F,C,D], !.

jugada_maquina(Tablero,Turno,Nivel,F,C,D,Tablero2,Turno2,Celdas):-
    copia_tablero(Tablero,TableroAux),
    minimax_inicial(TableroAux,Turno,Nivel,F,C,D),
    jugada_humano(Tablero,Turno,F,C,D,Tablero2,Turno2,Celdas), !.

% sugerencia_jugada(+Tablero,+Turno,+Nivel,?F,?C,?D)
% Utiliza la estrategia de minimax para calcular una buena jugada para sugerirle
% a un jugador humano.
sugerencia_jugada(Tablero,Turno,Nivel,F,C,D):-
    minimax_inicial(Tablero,Turno,Nivel,F,C,D).
