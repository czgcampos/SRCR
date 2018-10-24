%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao genealogica.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais
% Isto são questões, porque são condiçoes, sem conclusao

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,R -> {V,F}

soma(X,Y,R) :- R is X+Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,Z,R -> {V,F}

soma(X,Y,Z,R) :- R is X+Y+Z.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado somaC: LN,R -> {V,F}

soma([X],X).
soma([X|L],R) :-
	soma(L,R1),
	R is X+R1.

% soma([],0).
% soma([X|L],R) :-
%	soma(L,R1),
%	R is X+R1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado operacaoArit: X,Y,O,R -> {V,F}

operacaoArit(X,Y,+,R) :- R is X+Y.
operacaoArit(X,Y,-,R) :- R is X-Y.
operacaoArit(X,Y,*,R) :- R is X*Y.
operacaoArit(X,Y,/,R) :- 
	Y \= 0,
	R is X/Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maiorL: L,R -> {V,F}

maiorL([X],X).
maiorL([X|L],M) :-
	maiorL(L,R),
	maior(X,R,M).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado menorL: L,R -> {V,F}

menorL([X],X).
menorL([X|L],M) :-
	menorL(L,R),
	menor(X,R,M).