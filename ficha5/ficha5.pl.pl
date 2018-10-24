%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- dynamic vem_automovel/0.
:- dynamic vem_comboio/0.
:- dynamic '-'/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Atravessar a estrada?

atravessar( estrada ) :-
    nao( vem_automovel ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Atravessar a linha de caminho-de-ferro?

atravessar( ferrovia ) :-
    -vem_comboio.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado par: N -> {V,F,D}

par( 0 ).
par( X ) :-
    N is X-2,
    N >= 0,
    par( N ).
% tá mal a meu ver, porque se for um numero real, dá falso e não sabemos
% se um real é par ou impar
-par( X ) :-
    nao( par( X ) ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado impar: N -> {V,F,D}

impar( 1 ).
impar( X ) :-
    N is X-2,
    N >= 1,
    impar( N ).
-impar( 0 ).
-impar( X ) :-
    N is X-2,
    N >=0,
    -impar( N ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado arcoiris: Cor -> {V,F,D}

arcoiris(violeta).
arcoiris(vermelho).
-arcoiris(branco).
-arcoiris(preto).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nodo: N -> {V,F,D}

nodo(a).
nodo(b).
nodo(c).
nodo(d).
nodo(e).
nodo(f).
nodo(g).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado arco: O,D -> {V,F,D}

arco(b,a).
arco(b,c).
arco(c,a).
arco(c,d).
arco(f,g).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado terminal: N -> {V,F,D}

-terminal(N):-
    nodo(N),
    arco(N,D).
terminal(N):-
    nodo(N),
    nao(arco(N,D)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).