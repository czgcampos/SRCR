%-------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%-------------------------------------------------------------------
% Programacao em logica estendida

%-------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(unknown,fail).

%-------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op(900,xfy,'::').
:- dynamic '-'/1.
:- dynamic servico/2.
:- dynamic atoMedico/4.
:- dynamic feriado/1.

%-------------------------------------------------------------------
% Extensao do meta-predicado nao: Questao -> {V,F}

nao(Q):-
	Q,!,fail.
nao(Q).

%-------------------------------------------------------------------
% Extensao do meta-predicado evolucao: Termo -> {V,F}

evolucao(Termo):-
	solucoes(Invariante,+Termo::Invariante,Lista),
	insercao(Termo),
	teste(Lista).

insercao(Termo):-
	assert(Termo).
insercao(Termo):-
	retract(Termo),!,fail.

teste([]).
teste([R|LR]):-
	R,
	teste(LR).

%-------------------------------------------------------------------
% Extensao do meta-predicado involucao: Termo -> {V,F}

involucao(Termo):-
	solucoes(Invariante,+Termo::Invariante,Lista),
	remocao(Termo),
	teste(Lista).

remocao(T):-
	retract(T).

teste([]).
teste([R|LR]):-
	R,
	teste(LR).

%-------------------------------------------------------------------
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo(Questao,verdadeiro):-
	Questao.
demo(Questao,falso):-
	-Questao.
demo(Questao,desconhecido):-
	nao(Questao),
	nao(-Questao).

%-------------------------------------------------------------------
% Extensao do meta-predicado solucoes: Questao,Resposta -> {V,F}

solucoes(X,Y,Z):-
	findall(X,Y,Z).

%-------------------------------------------------------------------
% Extensao do meta-predicado comprimento: Questao,Resposta -> {V,F}

comprimento(S,N):-
	length(S,N).

%-------------------------------------------------------------------
%Extensão do predicado servico: servico, enfermeira -> {V,F,D}

servico("Ortopedia","Amelia").
servico("Obstetricia","Ana").
servico("Obstetricia","Maria").
servico("Obstetricia","Mariana").
servico("Geriatria","Sofia").
servico("Geriatria","Susana").
excepcao(servico(xpt007,"Teodora")).
excepcao(servico(np9,"Zulmira")).
nulo(np9).
+servico(S,"Zulmira"):-(solucoes((SS,"Zulmira"),(servico(SS,"Zulmira"),nao(nulo(SS))),L),
	comprimento(L,C),
	C==0).

%Extensão do predicado atoMedico: ato,enfermeira,utente,data -> {V,F,D}

atoMedico("Penso","Ana","Joana","sabado").
atoMedico("Gesso","Amelia","Jose","domingo").
excepcao(atoMedico(xpt017,"Mariana","Joaquina","domingo")).
excepcao(atoMedico("Domicilio","Maria",xpt121,xpt251)).
excepcao(atoMedico("Domicilio","Susana",X,"segunda")):-
	(X="Joase";X="Joao").
excepcao(atoMedico("Sutura",xpt313,"Josue","segunda")).
excepcao(atoMedico("Sutura",X,"Josefa",Y)):-
	(X="Maria";X="Mariana"),
	(Y="terca";Y="sexta").
excepcao(atoMedico("Penso","Ana","Jacinta",X)):-
	(X="segunda";X="terca";X="quarta";X="quinta";X="sexta").

%-------------------------------------------------------------------
%impede registos de atos medicos em feriados

feriado("domingo").
+atoMedico(A,M,U,D)::(solucoes((A,M,U,D),(atoMedico(A,M,U,D),feriado(D)),L),
	comprimento(L,C),
	C==0).

%-------------------------------------------------------------------
%impede remoção de profissionais com atos registados

-servico(S,M)::(solucoes((A),atoMedico(A,M,_,_),L),
	comprimento(L,C),
	C==0).