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
% Extensao do predicado pertence: L,E -> {V,F}

pertence([X|L],X).
pertence([H|L],X) :- 
	X\=H,
	pertence(L,X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: L,N -> {V,F}

comprimento([],0).
comprimento([X|L],N) :- 
	comprimento(L,M),
	N is 1+M.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado diferentes L,N -> {V,F}

diferentes([],0).
diferentes([X|L],N) :-
	pertence(L,X),
	diferentes(L,N).
% diferentes([X|L,NM]) :-
% 	\+(pertence(L,X)),
%	diferente(L,N),
%	NM is 1+N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar L1,L2,L -> {V,F}

concatenar([],L2,L2).
concatenar([X|L1],L2,[X|L]):-
	concatenar(L1,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sublista S,L -> {V,F}

sublista(S,L):-
	concatenar(X,S,Y),
	concatenar(Y,Z,L).