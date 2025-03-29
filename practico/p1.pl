% Ejercicio 1 -----------------------------------------------------------------
/*
Suponga definidos los siguientes predicados:
progenitor(X, Y) ← X es el progenitor (madre o padre) de Y.
distintos(X, Y) ← X e Y son diferentes.
casados(X, Y) ← X e Y están casados.
Defina los siguientes predicados:
hermano(X, Y) ← X e Y son hermanos.
tio(X, Y) ← X es tío(a) de Y.
tio_politico(X, Y) ← X es tío(a) político(a) de Y.
cuñado(X, Y) ← X e Y son cuñados.
concuñado(X, Y) ← X e Y son concuñados.
suegro(X, Y) ← X es suegro(a) de Y.
consuegro(X, Y) ← X e Y son consuegros.
En caso de ser necesarios otros predicados, defínalos.
*/

% Datos básicos para el ejercicio 1 -------------------------------------------
% Hechos básicos: progenitores
progenitor(juan, ana).
progenitor(juan, luis).
progenitor(maria, ana).
progenitor(maria, luis).
progenitor(carlos, pedro).
progenitor(carlos, sofia).
progenitor(luisa, pedro).
progenitor(luisa, sofia).
progenitor(ana, jose).
progenitor(ana, clara).
progenitor(roberto, jose).
progenitor(roberto, clara).

% Hechos básicos: parejas
casados(juan, maria).
casados(maria, juan).

casados(carlos, luisa).
casados(luisa, carlos).

casados(ana, roberto).
casados(roberto, ana).

casados(luis, sofia).
casados(sofia, luis).

% Hecho auxiliar: distintos
distintos(X, Y) :- X \= Y.
% Datos básicos para el ejercicio 1 -------------------------------------------

% hermano(X,Y)
hermano(X,Y) :- distintos(X,Y), progenitor(Z,X), progenitor(Z,Y).

% tio(X,Y)
tio(X,Y) :- progenitor(Z, Y), hermano(X,Z).

% tio_politico(X,Y)
tio_politico(X,Y) :- casados(Z,X), tio(Z,Y).

% cunado(X,Y)
cunado(X,Y) :- casados(X,Z), hermano(Z,Y).
cunado(X,Y) :- hermano(X,Z), casados(Z,Y).
/* Otra solución (equivalente)
cunado(X,Y) :- casados(Z,Y), hermano(X,Z).
cunado(X,Y) :- hermano(Z,Y), casados(X,Z).
*/

% concunado(X,Y)
concunado(X,Y) :- cunado(Z,Y), hermano(X,Z).
concunado(X,Y) :- cunado(Z,Y), casados(X,Z).


% Ejercicio 2 -----------------------------------------------------------------
/*
a) Indique si las siguientes unificaciones son válidas. ¿Cómo quedan los términos
unificados y la instanciación de variables luego de cada unificación?
i) X = juan.
ii) 42 = X.
iii) X = juan, 42 = X.
iv) X = juan, X = Y.
v) padre(Z,juan) = padre(jorge,juan).
vi) quiere(ana,X) = quiere(Y,pedro).
vii) quiere(ana,X) = quiere(X,pedro).
viii) g(f(X,Y),X,h(a))=g(f(b,Z),W,h(Z)).
ix) g(f(X,Y),X,h(a)) = g(f(b,Z),W,h(X)).
x) X = f(X).
xi) s(X,f(X)) = s(f(Z),Z).
xii) a(f(Y),W,g(Z)) = a(X,X,V).
xiii) b(f(Y),W,g(Z)) = b(V,X,V).
xiv) c(a,X,f(g(Y))) = c(Z,h(Z,W),f(W)).
b) ¿Qué ocurre en Prolog para los casos x y xi de la parte anterior? ¿Por qué?
*/

/*
v) padre(Z,juan) = padre(jorge,juan).
Z = jorge.

viii) g(f(X,Y),X,h(a))=g(f(b,Z),W,h(Z)).
X = b, Y = Z, W = b, Z = a.

ix) g(f(X,Y),X,h(a)) = g(f(b,Z),W,h(X)).
X = b, X = a => FALSE

xi) s(X,f(X)) = s(f(Z),Z).
X = f(Z), Z = f(X) => X = F(F(X))

xii) a(f(Y),W,g(Z)) = a(X,X,V).
W = F(Y), V = g(Z)

xiii) b(f(Y),W,g(Z)) = b(V,X,V).
F(Y) = g(Z) False

xiv) c(a,X,f(g(Y))) = c(Z,h(Z,W),f(W)).
Z = a, X = h(a,g(Y))
*/

% Ejercicio 3 -----------------------------------------------------------------
/*
Sea la siguiente definición de los números naturales:
nat(0).
nat(s(X)) :- nat(X).
Defina los siguientes predicados:
suma(X, Y, S) ← S es la suma de X e Y (S=X+Y).
resta(X, Y, R) ← R es la diferencia entre X e Y (X=Y+R)
producto(X, Y, P) ← P es el producto entre X e Y (P=X*Y)
distintos(X, Y) ← X e Y son distintos (X Y).
mayor(X, Y) ← X es mayor que Y (X>Y).
factorial(X, Y) ← Y es igual al factorial de X (Y=X!).
potencia(X, Y, Z) ← Z es igual a X elevado a la Y (Z = XY).
En todos los casos X, Y y Z son números naturales. En caso de ser necesarios otros
predicados auxiliares, defínalos.
*/

% Función auxiliar para convertir un número natural a su representación en números naturales
% nat(3, s(s(s(0))))
nat(0,0).
nat(s(X),Y) :- nat(X,Z), Y is Z + 1.
% -----------------------------------------------------------------------------

% suma(X,Y,S) ← S es la suma de X e Y (S=X+Y).
suma(X,0,X).
suma(X,s(Y),s(S)) :- suma(X,Y,S).

% resta(X,Y,R) ← R es la diferencia entre X e Y (X=Y+R)
resta(X,Y,R) :- suma(Y,R,X).

% producto(X,Y,P) ← P es el producto entre X e Y (P=X*Y)
producto(0,_,0).
producto(s(X), Y, P) :- producto(X,Y,W), suma(Y,W,P).

/* Se comenta para que no interfiera con el distintos ya definido
% distintos(X,Y) ← X e Y son distintos (X Y).
distintos(0, s(_)).
distintos(s(_), 0).
distintos(s(X), s(Y)) :- distintos(X,Y).
*/

% mayor(X,Y) ← X es mayor que Y (X>Y).
mayor(X,0) :- distintos(X,0).
mayor(s(X),s(Y)) :- mayor(X,Y).

% factorial(X,Y) ← Y es igual al factorial de X (Y=X!).
factorial(0,s(0)).
factorial(s(X),Y) :- factorial(X,Z), producto(Z,s(X),Y).

% potencia(X,Y,Z) ← Z es igual a X elevado a la Y (Z = XY).
potencia(s(X),0,s(0)):- distintos(X, 0).
potencia(X,s(Y),Z):- potencia(X,Y,W), producto(X,W,Z).

% Ejercicio 4 -----------------------------------------------------------------
/*
a) Defina los siguientes predicados sobre listas:

  largo(L, N) ← La lista L tiene N elementos (siendo N=sn(0)).

  ultimo(L,X) ← X es el último elemento de la lista L.

  sin_ultimo(L,S) ← S es la lista que se obtiene de suprimir el último elemento de L.

  reverso(L,R) ← La lista R contiene los elementos de la lista L en orden inverso.

  subsecuencia(L,S) ← La lista S contiene elementos (no necesariamente consecutivos) 
  de la lista L. Estos elementos preservan el orden de aparición que poseen en L.

  sublista(L,S) ← La lista S contiene elementos consecutivos de la lista L. Estos 
  elementos preservan el orden de aparición que poseen en L.

  prefijo(L, P) ← La lista P es un prefijo de la lista L.

  sufijo(L, S) ← La lista S es un sufijo de la lista L.

  borrar_todos(L,X,B) ← La lista B es la lista L sin ocurrencias del elemento X. 
  (Suponga que L es una lista de naturales definidos como en el ejercicio 2).

  sin_repetidos(L,S) ← La lista S es la lista L sin elementos repetidos. (Suponga que 
  L es una lista de naturales definidos como en el ejercicio 2).
*/

% largo(L, N) ← La lista L tiene N elementos (siendo N=sn(0)).
largo([],0).
largo([_|L], s(N)) :- largo(L,N).

% ultimo(L,X) ← X es el último elemento de la lista L.
ultimo([X], X).
ultimo([_,L],W) :- ultimo(L,W).

% sin_ultimo(L,S) ← S es la lista que se obtiene de suprimir el último elemento de L.
sin_ultimo([_],[]).
sin_ultimo([X|L],[X|S]) :- sin_ultimo(L,S).

% reverso(L,R) ← La lista R contiene los elementos de la lista L en orden inverso.
reverso_acc([], AC, AC).
reverso_acc([X|L],AC,R) :- reverso_acc(L,[X|AC],R).
reverso(X,L) :- reverso_acc(X,[],L).

% subsecuencia(L,S) ← La lista S contiene elementos (no necesariamente consecutivos) de la lista L. 
% Estos elementos preservan el orden de aparición que poseen en L.
subsecuencia(_,[]).
subsecuencia([X|L],[X|S]) :- subsecuencia(L,S).
subsecuencia([_|L],S) :- subsecuencia(L,S).

% sublista(L,S) ← La lista S contiene elementos consecutivos de la lista L.
sublista(L,S):- append(P,_,L), append(_,S,P).

% prefijo(L, P) ← La lista P es un prefijo de la lista L.
prefijo(L,S) :- append(S,_,L).
/* Otra solución (equivalente) sin usar append
prefijo(_,[]).
prefijo([X|L],[X|S]) :- prefijo(L,S).
*/

% sufijo(L, S) ← La lista S es un sufijo de la lista L.
sufijo(L,S) :- append(_,S,L).
/* Otra solución (equivalente) sin usar append
sufijo(L,L).
sufijo([_|L],S) :- sufijo(L,S).
*/

% borrar_todos(L,X,B) ← La lista B es la lista L sin ocurrencias del elemento X.
borrar_todos([],_,[]).
borrar_todos([X|T], X, B) :- borrar_todos(T,X,B).
borrar_todos([Y|T], X, [Y|B]) :- borrar_todos(T,X,B), distintos(Y,X).

% sin_repetidos(L,S) ← La lista S es la lista L sin elementos repetidos.
% utilizando acumulador
sin_acc([], Ac, Ac).
sin_acc([X|T], Ac, S) :- member(X,Ac), sin_acc(T,Ac,S).
sin_acc([X|T],Ac,S) :- \+ member(X,Ac), append(Ac,[X],Ac1), sin_acc(T,Ac1,S).

sin_repetidos(L,S) :- sin_acc(L,[], S).

/*
b) Suponga que utilizamos listas para representar conjuntos. Se deben cumplir dos
propiedades para esto. En primer lugar, no pueden repetirse elementos en la
lista. En segundo lugar, no importa el orden de aparición de un elemento en la
lista. Teniendo en cuenta esto, defina los siguientes predicados sobre conjuntos
(de naturales) representados mediante listas:

  conjunto(C) ← La lista C representa un conjunto.

  conj_iguales(C1, C2) ← Las listas C1 y C2 representan el mismo conjunto.

  subconjunto(C, S) ← La lista S representa un subconjunto del representado por la lista C.

  intersección(C1, C2, I) ← La lista I representa el conjunto intersección de los conjuntos 
  representados por las listas C1 y C2.

  union(C1, C2, U) ← La lista U representa el conjunto unión de los conjuntos representados 
  por las listas C1 y C2.
*/

% conjunto(C) ← La lista C representa un conjunto.
conjunto(L) :- sin_repetidos(L,L).

% conj_iguales(C1, C2) ← Las listas C1 y C2 representan el mismo conjunto.
conj_iguales(L1,L2) :- length(L1, X), length(L2, X), append(L1,L2,L3), sin_repetidos(L3,S), length(S,X),
% verificar que L1 y L2 son conjuntos (puede no ser necesario)
conjunto(L1), conjunto(L2).

% subconjunto(C, S) ← La lista S representa un subconjunto del representado por la lista C.
/* ADVERTENCIA:
Funcionan regulinchi generando subconjuntos. (no funcionan pa eso). */
subconjunto(_, []).
subconjunto(C, [X|S]) :- 
    select(X, C, C1), % Extrae X de C y genera una nueva lista C1 sin X
    subconjunto(C1, S).
/* Otra solución usando member.
subconjunto(_,[]).
subconjunto(C,[X|S]) :- member(X,C), subconjunto(C,S).
*/

% intersección(C1, C2, I) ← La lista I representa el conjunto intersección de los conjuntos
% representados por las listas C1 y C2.
interseccion(C1, C2, I) :- inter_acc(C1,C2,[],I).
inter_acc([],_, Ac, Ac).
inter_acc([X|C1],C2,Ac,I) :-  member(X,C2), inter_acc(C1,C2,[X|Ac],I).
inter_acc([X|C1],C2,Ac,I) :- \+ member(X,C2), inter_acc(C1,C2,Ac,I).

% union(C1, C2, U) ← La lista U representa el conjunto unión de los conjuntos representados
% por las listas C1 y C2.
union(C1,C2,U) :- uni_acc(C1,C2,U).
uni_acc([],Ac,Ac).
uni_acc([X|C1],Ac,U):- \+ member(X,Ac), uni_acc(C1,[X|Ac],U).
uni_acc([X|C1],Ac,U):- member(X,Ac), uni_acc(C1,Ac,U).
/* Otra solución (equivalente) mas simple
union(C1,C2,U) :- append(C1,C2,C3), sin_repetidos(C3, U).
*/

% Ejercicio 5 -----------------------------------------------------------------
/*
Defina el siguiente predicado sin utilizar predicados auxiliares, y utilizando
únicamente 3 reglas o hechos:

  sumaLista(Ns, N) ← N es la suma de los elementos de la lista de naturales Ns 
  (los naturales están representados como en el Ej.2: N sn(0)).
*/
sumaLista([],0).                                             
sumaLista([0|Ns],N):- sumaLista(Ns,N).
sumaLista([s(X)|Ns],s(N)):- sumaLista([X|Ns],N).

% Ejercicio 6 -----------------------------------------------------------------
/*
Escriba el predicado ancestro(X, Y, L), que retorna una lista L con la línea de
descendencia entre un ancestro X y su descendiente Y. Suponga definido el
predicado progenitor, idéntico al de ejercicio 1 de este práctico.
Por ejemplo:
progenitor(juan, jose).
progenitor(jose, pedro).
progenitor(pedro, maria).
Si la consulta es ancestro(juan, maria, L), la lista L tiene los elementos [juan,jose,
pedro,maria].
*/
/* Datos extra para probar solución se comentan para que no interfieran con ejercicio 1
progenitor(juan, jose).
progenitor(jose, pedro).
progenitor(pedro, maria).
progenitor(juan, juana).
progenitor(juana, juana2).
progenitor(juana2, maria).
% WTF pedro y juana2
*/
ancestro(X,Y,[X|[Y]]):- progenitor(X,Y).
ancestro(X,Y,[X|L]):- progenitor(X,Z), ancestro(Z,Y,L).

% Ejercicio 7 -----------------------------------------------------------------
/*
Sea la siguiente definición de árbol binario (AB):

arbol_bin(Arbol) ← Arbol es un árbol binario

  arbol_bin(vacio).
  arbol_bin(arbol(Raiz, Izq, Der)) :- arbol_bin(Izq), arbol_bin(Der).

Defina los siguientes predicados sobre árboles binarios de naturales:

  member_ab(X, A) ← X es un elemento del AB A.
  cambiar(X, Y, Ax, Ay) ← El AB Ay se logra al cambiar en el AB Ax los X's por Y's.
  sumar(A, S) ← S tiene el valor de la suma de los elementos del AB A.
  abb(A) ← El AB A es un árbol binario de búsqueda (ABB).
*/

% member_ab(X, A) ← X es un elemento del AB A.
member_ab(X,arbol(X,_,_)).
member_ab(X,arbol(_,I,_)):- member_ab(X,I).
member_ab(X,arbol(_,_,D)):- member_ab(X,D).
/* Datos de prueba:
member_ab(A,arbol(1, arbol(2, arbol(4, vacio, vacio), arbol(5, vacio, vacio)), arbol(3, arbol(6, vacio, vacio), arbol(7, vacio, vacio)))).
    1
   / \
  2   3
 / \ / \
4  5 6  7
*/

% cambiar(X, Y, Ax, Ay) ← El AB Ay se logra al cambiar en el AB Ax los X's por Y's.
cambiar(_, _, vacio,vacio).
cambiar(X, Y, arbol(X,Ix,Dx), arbol(Y,Iy,Dy)):- cambiar(X,Y,Ix,Iy), cambiar(X,Y,Dx,Dy).
cambiar(X, Y, arbol(Z,Ix,Dx), arbol(Z,Iy,Dy)):- X \= Z, cambiar(X,Y,Ix,Iy), cambiar(X,Y,Dx,Dy).
/* Datos de prueba:
cambiar(1,4,arbol(1, arbol(2, arbol(4, vacio, vacio), arbol(5, vacio, vacio)), arbol(3, arbol(6, vacio, vacio), arbol(7, vacio, vacio))),A).
    1
   / \
  2   3
 / \ / \
4  5 6  7
*/

% sumar(A, S) ← S tiene el valor de la suma de los elementos del AB A.
sumar(vacio,0).
sumar(arbol(R,I,D),S):- sumar(I,X),sumar(D,Y), S is X+Y+R.
/* Datos de prueba:
sumar(arbol(1, arbol(2, arbol(4, vacio, vacio), arbol(5, vacio, vacio)), arbol(3, arbol(6, vacio, vacio), arbol(7, vacio, vacio))),A).
    1
   / \
  2   3
 / \ / \
4  5 6  7
*/

% abb(A) ← El AB A es un árbol binario de búsqueda (ABB).
abb(vacio).
abb(arbol(_,vacio,vacio)).
abb(arbol(R,arbol(Ri,Ii,Di),vacio)):- Ri < R, abb(arbol(Ri,Ii,Di)).
abb(arbol(R,vacio,arbol(Rd,Id,Dd))):- Rd > R, abb(arbol(Rd,Id,Dd)).
abb(arbol(R,arbol(Ri,Ii,Di),arbol(Rd,Id,Dd))):- Ri < R, Rd > R, abb(arbol(Ri,Ii,Di)), abb(arbol(Rd,Id,Dd)).

/* Datos de prueba:
abb(arbol(1, arbol(2, arbol(4, vacio, vacio), arbol(5, vacio, vacio)), arbol(3, arbol(6, vacio, vacio), arbol(7, vacio, vacio)))).
    1
   / \
  2   3
 / \ / \
4  5 6  7
abb(arbol(1,vacio,vacio)).
  1
abb(arbol(2,arbol(1,vacio,vacio),vacio)).
  2
 /
1
abb(arbol(2,arbol(1,vacio,vacio),arbol(3,vacio,vacio))).
  2
 / \
1   3
abb(arbol(1,vacio,arbol(3,vacio,vacio))).
  2
   \
    3
abb(arbol(2,arbol(1,vacio,vacio),arbol(3,vacio,arbol(5,arbol(4,vacio,vacio),vacio)))).
  2
 / \
1   3
     \
      5
     /
    4
abb(arbol(50, arbol(25, arbol(12, arbol(6, vacio, vacio), arbol(18, vacio, vacio)), arbol(37, arbol(31, vacio, vacio), arbol(43, vacio, vacio))), arbol(75, arbol(62, arbol(56, vacio, vacio), arbol(68, vacio, vacio)), arbol(87, arbol(81, vacio, vacio), arbol(93, vacio, vacio))))).
          50
        /    \
     25        75
    /  \      /   \
  12   37    62   87
 / \  / \   / \   / \
6 18 31 43 56 68 81 93
*/

% Ejercicio 8 -----------------------------------------------------------------
/*
Un alfabeto  es un conjunto finito de símbolos, y  la tira vacía. Se define el
conjunto de expresiones regulares (e.r.) sobre  de la siguiente forma:

     0 es una e.r. que denota al conjunto vacío, 0  
     e es una e.r. que denota al conjunto {}, e  
     a es una e.r. que denota al conjunto {a}, a
     Dadas r y s, e.r. sobre  que denotan a LR y LS respectivamente:
        i. (r|s) es una e.r. que denota a (LR  LS)
        ii. (r.s) es una e.r. que denota (LR . LS)
        iii. (r*) es una e.r. que denota a LR*.
     Estas son todas las e.r. sobre .

En donde la operación . es la concatenación, y * es la clausura de Kleene de lenguajes.
Defina los siguientes predicados:

sigma(S) ← S es el alfabeto de las e.r.
exp_reg(S, R) ← R es una e.r. sobre el alfabeto S.
pertenece(S, R, X) ← X es una tira denotada por la expresión regular R sobre el alfabeto S.
*/

% sigma(S) ← S es el alfabeto de las e.r.

% exp_reg(S, R) ← R es una e.r. sobre el alfabeto S.

% pertenece(S, R, X) ← X es una tira denotada por la expresión regular R sobre el alfabeto S.
