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
% Extensao do predicado morcego: A -> {V,F,D}

morcego(batemene).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pinguim: A -> {V,F,D}

pinguim(pingu).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avestruz: A -> {V,F,D}

avestruz(trux).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cao: A -> {V,F,D}

cao(boby).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado mamifero: A -> {V,F,D}

mamifero(silvestre).
mamifero(A):-
    cao(A).
mamifero(A):-
    gato(A).
mamifero(A):-
    morcego(A).
-mamifero(A):-
    ave(A).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado canario: A -> {V,F,D}

canario(piupiu).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ave: A -> {V,F,D}

ave(pitigui).
ave(A):-
    canario(A).
ave(A):-
    periquito(A).
ave(A):-
    avestruz(A).
ave(A):-
    pinguim(A).
-ave(A):-
    mamifero(A).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado voar: A -> {V,F,D}

-voar(tweety).
-voar(A):-
    avestruz(A).
-voar(A):-
    pinguim(A).
voar(A):-
    morcego(A).
-voar(A):-
    mamifero(A).
voar(A):-
    ave(A).

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