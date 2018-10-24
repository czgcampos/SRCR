%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida
%
% Representacao de conhecimento imperfeito

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic filho/2.
:- dynamic pai/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

%Ficha 7

-jogo(I,A,C) :- nao(jogo(I,A,C)), nao(excecao(jogo(I,A,C))).

%iii

excecao(jogo(3,cc,500)).
excecao(jogo(3,cc,2500)).

%iv 

jogo(4,dd,xpto4000).
excecao(jogo(I,A,C)) :- jogo(I,A,xpto4000).

%v 

jogo(5,ee,xpto5000).
excecao(jogo(I,A,C)) :- jogo(I,A,xpto5000).
nulo(xpto5000).

+jogo(I,A,C) :: (solucoes((I,A,Cs),jogo(4,ee,Cs),nao(nulo(Cs)),S), comprimento(S,N), N==0).

%vi

jogo(6,ff,250).

excecao(jogo(6,ff,X)) :- X>=5000.

%vii

-jogo(7,gg,2500).
jogo(7,gg,xpto7000).
excecao(jogo(I,A,C)) :- jogo(I,A,xpto7000).

%vii

jogo(8,hh,1000).

%ix

excecao(jogo(9,ii,3000)).