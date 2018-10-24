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

:- dynamic filho/2.
:- dynamic pai/2.
:- dynamic mae/2.
:- dynamic irmao/2.
:- dynamic nascer/4.
:- op(900,xfy,'::').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado evolucao: Termo -> {V,F}

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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido

+pai( P,F ) :: (solucoes( (P,F),(pai( P,F )),S ),
                  comprimento( S,N ), N == 1
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Referencial: nao admitir mais do que 1 pai para um mesmo individuo

+pai( P,F ) :: (solucoes( (Ps),(pai( Ps,F )),S ),
                  comprimento( S,N ), N =< 1
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido

+mae( M,F ) :: (solucoes( (M,F),(pai( M,F )),S ),
                  comprimento( S,N ), N == 1
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Referencial: nao admitir mais do que 1 mãe para um mesmo individuo

+mae( M,F ) :: (solucoes( (Ms),(pai( Ms,F )),S ),
                  comprimento( S,N ), N =< 1
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: P,F -> {V,F,D}

-pai( P,F ) :-
    nao( pai( P,F ) ),
    nao( excecao( pai( P,F ) ) ).
pai(abel,ana).
pai(antonio,anibal).
pai(bras,berta).
pai(bras,berto).
excecao(pai(celso,crispim)).
excecao(pai(caio,crsipim)).
pai(elias, eurico).
excecao(pai(fausto,fabia)).
excecao(pai(fausto,otavia)).
pai(guido,golias).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado mae: M,F -> {V,F,D}

-mae( M,F ) :-
    nao( mae( M,F ) ),
    nao( excecao( mae( M,F ) ) ).
mae(alice,ana).
mae(alberta,anibal).
mae(belem,berta).
mae(belem,berto).
mae(catia,crispim).
excecao(mae(M,F)) :- mae(xpto806,F).
mae(xpto806, danilo).
mae(elsa, eurico).
mae(xpto808,fabia).
mae(xpto808,otavia).
excecao(mae(M,F)) :- mae(xpto808,F).
mae(guida,golias).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nascer: P,D,M,A -> {V,F,D}

nascer(ana,01,01,2010).
nascer(anibal,2,1,2010).
nascer(berto,2,2,2010).
nascer(berta,2,2,2010).
nascer(catia,3,3,2010).
nascer(danilo,4,4,2010).
-nascer(C,D,M,A) :- nao(nascer(C,D,M,A)), nao(excecao(nascer(C,D,M,A))).
excecao(nascer(eurico, 05, 05, 2010)).
excecao(nascer(eurico, 15, 05, 2010)).
excecao(nascer(eurico, 25, 05, 2010)).
nascer(golias, xpto809, xpto890, xpto899).
excecao(nascer(C,D,M,A)) :- nascer(C,xpto809,xpto890,xpto899).
nulo(xpto809).
nulo(xpto890).
nulo(xpto899).
+nascer(C,D,M,A) :: (solucoes((Dia,Mes,Ano), (nascer(golias, Dia,Mes,Ano)), nao(nulo(Dia,Mes,Ano)), S),
                    comprimento(S,N), N==0).
nascer(helder, xpto810, xpto818, xpto819).
excecao(nascer(C,D,M,A)) :- nascer(C,xpto810,xpto818,xpto819).
-nascer(helder, 08, 08, 2010).