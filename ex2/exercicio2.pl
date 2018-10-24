%-----------------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
% Exercicio prático 2

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op(900,xfy,'::').

% Para os invariantes:

:- dynamic '-'/1.
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic recibo/9.

% Para a conjunção e disjunção de demo

:- op(900,xfy,'e').
:- op(900,xfy,'ou').



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representaçao de conhecimento perfeito positivo
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Extensao do predicado utente: IdUt,Nome,Idade,Morada -> {V,F,D}
utente(1, carlos, 21, famalicao).
utente(2, diana, 20, trofa).
utente(3, marcos, 20, anais).
utente(4, vitor, 20, guimaraes).
utente(5, antonio, 6, povoadevarzim).
utente(6, rita, 26, pontedelima).
utente(7, marta, 66, guimaraes).
utente(8, paulo, 16, barcelos).
utente(9, maria, 43, barcelos).
utente(10, luis, 51, vizela).

% Extensao do predicado prestador: IdPrest,Nome,Especialidade,Instituicao -> {V,F,D}
prestador(1, pres1, pediatria, csjoane).
prestador(2, pres2, cardiologia, hospitalbraga).
prestador(3, pres3, cirurgia, hospitalbraga).
prestador(4, pres4, ginecologia, hospitalbraga).
prestador(5, pres5, neurologia, hsmm).
prestador(6, pres6, psiquiatria, hsog).
prestador(7, pres7, oftamologia, htrofa).
prestador(8, pres8, reumatologia, htrofa).
prestador(9, pres9, psiquiatria, hospitalbraga).

% Extensao do predicado cuidado: Data,IdUt,IdPrest,Descricao,Custo -> {V,F,D}
cuidado(01-02-2017, 1, 6, hipnose, 15).
cuidado(13-02-2017, 3, 4, papanico, 30).
cuidado(13-02-2017, 2, 5, cerebrotroca, 30).
cuidado(14-02-2017, 2, 7, olhoterapia, 7).
cuidado(20-03-2017, 4, 2, pacemaker, 20).
cuidado(02-04-2017, 7, 4, ovariologia, 5).
cuidado(03-04-2017, 3, 5, neuroterapia, 25).
cuidado(20-04-2017, 6, 7, retinoterapia, 35).
cuidado(22-05-2017, 6, 2, cardioterapia, 55).
cuidado(04-06-2017, 2, 2, cardiograma, 99).
cuidado(15-06-2017, 1, 1, terapiafala, 5).
cuidado(18-06-2017, 2, 8, reumatomagrafia, 350).
cuidado(18-06-2017, 2, 9, cbt, 10).

% Extensao do predicado recibo: IdRecibo, IdUt, NomeUt, Morada, Especialidade, Instituicao, Data, Descricao, Custo -> {V,F,D}
recibo(1, 3, marcos, anais, ginecologia, hospitalbraga, 13-02-2017, papanico, 30).
recibo(2, 4, vitor, guimaraes, cardiologia, hospitalbraga, 20-03-2017, pacemaker, 20).



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representaçao de conhecimento perfeito negativo
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% Definições das negações fortes (não é verdadeiro ^ não é desconhecido)

-utente(Id,N,I,M) :- 
					nao(utente(Id,N,I,M)),
					nao(excecao(utente(Id,N,I,M))).

-prestador(Id,N,E,I) :-
					nao(prestador(Id,N,E,I)),
					nao(excecao(prestador(Id,N,E,I))).

-cuidado(D,IdU,IdP,De,C) :-
					nao(cuidado(D,IdU,IdP,De,C)),
					nao(excecao(cuidado(D,IdU,IdP,De,C))).

-recibo(IdR, IdU, N, M, E, I, D, De, C) :-
					nao(recibo(IdR, IdU, N, M, E, I, D, De, C)),
					nao(excecao(recibo(IdR, IdU, N, M, E, I, D, De, C))).


% Negação explícita

-utente(13,joaquim,65,braganca).



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representar casos de conhecimento imperfeito incerto
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Desconhece-se a morada do utente com o id=12, correspondente à Mariana de 43 anos. 
utente(12, mariana, 43, i1).
excecao( utente( A, B, C, D ) ) :-
	utente( A, B, C, i1).

% ----------------------------------------------------------------------------------------------------
% Não se sabe qual é o nome do prestador com o id=11 do hospital de braga,
% nem qual a especialidade que o mesmo exerce. 
prestador(11, i2, i3, hospitalbraga).
excecao( prestador( A, B, C, D ) ) :-
	prestador( A, i2, i3, D).



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representar casos de conhecimento imperfeito impreciso
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se sabe se a utente com id=11, com o nome Inês, residente em Lisboa, tem 23 ou 24 anos.
excecao( utente(11,ines,23,lisboa) ).
excecao( utente(11,ines,24,lisboa) ).

% ----------------------------------------------------------------------------------------------------
% Desconhece-se se o prestador com id=10, pres10, exerce dermatologia no Hospital de Braga ou no Hospital de São João.
excecao( prestador(10, pres10, dermatologia, hospitalbraga) ).
excecao( prestador(10, pres10, dermatologia, hsaojoao) ).

% ----------------------------------------------------------------------------------------------------
% Não se sabe se o cuidado "penso" prestado em 19-12-2017, pelo prestador com id=8 e com o 
% valor monetário de 10€ corresponde ao utende com id=2 ou id=3.
excecao( cuidado(19-12-2017, 2, 8, penso, 10) ).
excecao( cuidado(19-12-2017, 3, 8, penso, 10) ).

% ----------------------------------------------------------------------------------------------------
% Desconhece-se se o cuidado prestado em 12-05-2017 pelo prestador com id=5 ao utente com id=8 foi
% quimioterapia ou radioterapia, e se o custo foi 50€ ou 70€.
excecao( cuidado(12-05-2017, 8, 5, radioterapia, 50) ).
excecao( cuidado(12-05-2017, 8, 5, radioterapia, 70) ).
excecao( cuidado(12-05-2017, 8, 5, quimioterapia, 50) ).
excecao( cuidado(12-05-2017, 8, 5, quimioterapia, 70) ).

% ----------------------------------------------------------------------------------------------------
% Existe um recibo (idRecibo=3) do utente com id=2, intitulado Diana, que vive na Trofa e efetuou,
% no hospital da trofa, uma consulta de rotina na área da nutrição, com custo de 50€.
% Desconhece-se, no entanto, se o recibo foi emitido em 27-03-2017 ou 28-03-2017.
excecao( recibo(3, 2, diana, trofa, nutricao, htrofa, 27-03-2017, rotina, 50) ).
excecao( recibo(3, 2, diana, trofa, nutricao, htrofa, 28-03-2017, rotina, 50) ).

% ----------------------------------------------------------------------------------------------------
% Existe um recibo (idRecibo=5) do utente com id=10, intitulado luis, que vive em vizela e efetuou,
% no hospital de braga, hipnose na área da psiquiatria, na data 02-11-2017.
% Desconhece-se o custo declarado no recibo, no entanto sabe-se que se situa entre 35€ e 60€.
recibo(5, 10, luis, vizela, psiquiatria, hbraga, 02-11-2017, hipnose, i4).
excecao( recibo(A, B, C, D, E, F, G, H, I) ) :-
				recibo(A, B, C, D, E, F, G, H, i4),
				I > 35, I < 60.



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% 					Representar casos de conhecimento imperfeito interdito
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Não se pode saber qual a descrição do cuidado realizado a 13-08-2017, prestado pelo prestador com id=7
% ao utente com id=10, sendo que o custo foi de 45€.
cuidado( 13-08-2017, 10, 7, i5, 45).
excecao( cuidado(A, B, C, D, E) ) :-
	cuidado(A, B, C, i5, E).
nulointerdito( i5 ).
+cuidado( A, B, C, D, E ) :: ( 	solucoes(
											(A,B,C,Interdito,E),
											(cuidado( 13-08-2017, 10, 7, Interdito, 45), nao(nulointerdito(Interdito))),
											List
								),
                 		  		comprimento( List, N ),
                 		  		N == 0
                 		  	).

% ----------------------------------------------------------------------------------------------------
% Nunca se poderá saber, a partir do recibo com id=4, em que instituição foi prestado um peeling químico,
% na área da dermatologia, ao utente com id=7 e intitulado Marta, em 04-12-2017, com o custo de 30€.
recibo( 4, 7, marta, guimaraes, dermatologia, i6, 04-12-2017, peelingquimico, 30 ).
excecao( recibo(A, B, C, D, E, F, G, H, I) ) :-
	recibo(A, B, C, D, E, i6, G, H, I).
nulointerdito( i6 ).
+recibo(A, B, C, D, E, F, G, H, I) :: ( solucoes(
													(A, B, C, D, E, Interdito, G, H, I),
													(recibo( 4, 7, marta, guimaraes, dermatologia, Interdito, 04-12-2017, peelingquimico, 30), nao(nulointerdito(Interdito))),
													List
										),
                 		  	   			comprimento( List, N ),
                 		  	  		 	N == 0
                 		  	   			).



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% Manipular invariantes que designem restrições à inserção e à remoção de conhecimento POSITIVO do sistema
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um utente com um id já existente
% uso _ quando o dado năo é importante e năo quero saber dele
+utente(Id, N, I, M) :: (solucoes(Id, utente(Id,_,_,_), S),
						comprimento(S,L),
						L==1).

% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um prestador com um id já existente
+prestador(Id, N, E, I) :: (solucoes(Id, prestador(Id,_,_,_), S),
							comprimento(S, L),
							L == 1).

% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um cuidado quando o id do utente/prestador
% năo existem na base de conhecimento
+cuidado(Dat,U,P,D,C) :: (solucoes(P, prestador(P, _, _, _), LisP),
						comprimento(LisP, NumP),
						NumP == 1,
						solucoes(U, utente(U, _, _, _), LisU),
						comprimento(LisU, NumU),
						NumU == 1).

% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um recibo com um id já existente
% uso _ quando o dado năo é importante e năo quero saber dele
+recibo(Id, U, N, M, E, I, Dat, D, C) :: (solucoes(Id, recibo(Id,_,_,_,_,_,_,_,_), R),
					comprimento(R,Lr),
					Lr==1,
					solucoes((U,N,M), utente(U,N,_,M), Uts),
					comprimento(Uts,Lu),
					Lu==1,
					solucoes((P,E,I), prestador(P,_,E,I), Pres),
					comprimento(Pres, Lp),
					Lp>=1,
					solucoes((Dat,U,P,D,C), cuidado(Dat,U,P,D,C), Cuids),
					comprimento(Cuids, Lc),
					Lc==1).


% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um utente năo presente na base de conhecimento
% e com id associado a cuidado
-utente(Id, N, I, M) :: (solucoes(Id, utente(Id,N,I,M), Uts),
						comprimento(Uts, Lu),
						Lu==1,
						solucoes(Id, cuidado(_,Id,_,_,_), Cuids),
						comprimento(Cuids, Lc),
						Lc == 0).


% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um prestador năo presente na base de
% conhecimento e com o id associado a cuidado
-prestador(Id, N, E, I) :: (solucoes(Id, prestador(Id,_,_,_), Prests),
							comprimento(Prests, Lp),
							Lp == 1,
							solucoes(Id, cuidado(_,_,Id,_,_), Cuids),
							comprimento(Cuids, Lc),
							Lc == 0).


% ----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um cuidado năo presente na base de conhecimento
-cuidado(Dat, U, P, D, C) :: (solucoes((Dat,U,P,D,C), cuidado(Dat,U,P,D,C), Cuids),
							comprimento(Cuids, L),
							L == 1).


% ----------------------------------------------------------------------------------------------------
% Invariante que não permite a remoção de qualquer recibo, uma vez que a informação
% financeira nunca pode ser eliminada. Anti-fraude.
-recibo(Id, U, N, M, E, I, Dat, D, C) :: fail.



% ////////////////////////////////////////////////////////////////////////////////////////////////////
% ////////////////////////////////////// Predicados importantes //////////////////////////////////////
% ////////////////////////////////////////////////////////////////////////////////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensao do meta-predicado demo: Questao,Resposta -> {verdadeiro, falso, desconhecido}
demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demoL: ListaQuestoes,ListaRespostas -> {V,F,D}
% Responde individualmente às questões de uma lista de questões
demoL([],[]).
demoL([Q|TQ],[R|TR]) :- demo(Q,R), demoL(TQ,TR).

% ----------------------------------------------------------------------------------------------------
% Extensao do meta-predicado demoComp: ConjQuestoes,Resposta -> {verdadeiro, falso, desconhecido}
% Trata de fazer o demo de disjunção, conjunção ou nada
demoComp(Q1 e Q2, R) :-
					demo(Q1,R1),
					demoComp(Q2,R2),
					conjuncao(R1,R2,R).
demoComp(Q1 ou Q2, R) :-
					demo(Q1,R1),
					demoComp(Q2,R2),
					disjuncao(R1,R2,R).
demoComp(Q1, R) :- demo(Q1,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado conjuncao: ValorLogico,ValorLogico,ValorLogico -> {V,F,D}
% O terceiro argumento corresponde ao valor lógico correspondente à conjunção dos valores lógicos dos dois primeiros argumentos
% Entendem-se valores lógicos como: verdadeiro, falso ou desconhecido
conjuncao(verdadeiro,verdadeiro,verdadeiro).
conjuncao(verdadeiro,desconhecido,desconhecido).
conjuncao(verdadeiro,falso,falso).
conjuncao(desconhecido,verdadeiro,desconhecido).
conjuncao(desconhecido,desconhecido,desconhecido).
conjuncao(desconhecido,falso,falso).
conjuncao(falso,_,falso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado disjuncao: ValorLogico,ValorLogico,ValorLogico -> {V,F,D}
% O terceiro argumento corresponde ao valor lógico correspondente à conjunção dos valores lógicos dos dois primeiros argumentos
% Entendem-se valores lógicos como: verdadeiro, falso ou desconhecido
disjuncao(verdadeiro,verdadeiro,verdadeiro).
disjuncao(verdadeiro,desconhecido,verdadeiro).
disjuncao(verdadeiro,falso,verdadeiro).
disjuncao(desconhecido,verdadeiro,verdadeiro).
disjuncao(desconhecido,desconhecido,desconhecido).
disjuncao(desconhecido,falso,desconhecido).
disjuncao(falso,verdadeiro,verdadeiro).
disjuncao(falso,desconhecido,desconhecido).
disjuncao(falso,falso,falso).



%-----------------------------------------------------------------------------------------------------
% Evolução do conhecimento 
% permite adicionar conhecimento ou atualizar conhecimento imperfeito
%-----------------------------------------------------------------------------------------------------

% Extensao do predicado utente: IdUt,Nome,Idade,Morada -> {V,F,D}
% Extensao do predicado prestador: IdPrest,Nome,Especialidade,Instituicao -> {V,F,D}
% Extensao do predicado cuidado: Data,IdUt,IdPrest,Descricao,Custo -> {V,F,D}
% Extensao do predicado recibo: IdRecibo, IdUt, NomeUt, Morada, Especialidade, Instituicao, Data, Descricao, Custo -> {V,F,D}


evolucao(utente(Id,Nome,Idade,Morada)):-
		demo(utente(Id,Nome,Idade,Morada),desconhecido),
		solucoes(utente(Id,N,I,M), utente(Id,N,I,M),L),
		remocaoL(utente(Id,Nome,Idade,Morada),L).

evolucao(utente(Id,Nome,Idade,Morada)):-
		demo(utente(Id,Nome,Idade,Morada),falso),
		inserir(utente(Id,Nome,Idade,Morada)).

evolucao(prestador( Id,Nome,Especialidade,Instituicao ) ):-
		demo(prestador(Id,Nome,Especialidade,Instituicao),desconhecido),
		solucoes(prestador(Id,N,E,I), prestador(Id,N,E,I),L),
		remocaoL(prestador(Id,Nome,Especialidade,Instituicao),L).

evolucao(prestador( Id,Nome,Especialidade,Instituicao ) ):-
		demo(prestador(Id,Nome,Especialidade,Instituicao),falso),
		inserir(prestador(Id,Nome,Especialidade,Instituicao)).

evolucao(cuidado(Data,IdU,IdP,Descricao,Custo)):-
		demo(cuidado(Data,IdU,IdP,Descricao,Custo),desconhecido),
		solucoes(cuidado(Data,IdU,IdP,Descricao,Custo), cuidado(Data,IdU,IdP,Descricao,Custo),L),
		remocaoL(cuidado(Data,IdU,IdP,Descricao,Custo),L).

evolucao(cuidado(Data,IdU,IdP,Descricao,Custo)):-
		demo(cuidado(Data,IdU,IdP,Descricao,Custo),falso),
		inserir(cuidado(Data,IdU,IdP,Descricao,Custo)).

evolucao(recibo(Data,IdU, IdS,Custo)):-
		demo(recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo),desconhecido),
		solucoes(recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo), recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo),L),
		remocaoL(recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo),L).

evolucao(recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo)):-
		demo(recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo),falso),
		inserir(recibo(IdR, IdU, NomeU, Morada, Especialidade, Instituicao, Data, Descricao, Custo)).







% ////////////////////////////////////////// Predicados Extra ////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado inserir: Termo -> {V,F}
% mesmo que EVOLUCAO da PARTE 1

inserir(Termo) :-
	solucoes(Invariante, +Termo::Invariante, Lista),
	insercao(Termo),
	teste(Lista).


% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado remover: Termo -> {V,F}

remover(Termo) :-
	solucoes(Invariante, -Termo::Invariante, Lista),
	teste(Lista),
	remocao(Termo).


% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado insercao: Termo -> {V,F}

insercao(T) :-
	assert(T).
insercao(T) :-
	retract(T),!,fail.


% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado remocao: Termo -> {V,F}

remocao(T) :-
	retract(T).
remocao(T) :-
	assert(T),!,fail.


% ----------------------------------------------------------------------------------------------------
% Extensão do predicado que permite a remover conhecimento de uma lista

remocaoL( Termo,L ) :-
    retractL( L ),
    inserir(Termo).
remocaoL( Termo,L) :-
    assertL( L ),!,fail.

retractL([]).
retractL([X|L]):- 
		retract(X), 
		retractL(L).

assertL([]).
assertL([X|L]):- 
		assert(X), 
		assertL(L).


% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado teste: Lista -> {V,F}

teste([]).
teste([I|L]) :-
	I,
	teste(L).


% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado solucoes: X,Y,Z -> {V,F,D}

solucoes(X,Y,Z) :-
	findall(X,Y,Z).


% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado comprimento: Lista, Resultado -> {V,F}

comprimento(X,Z):-
	length(X,Z).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado somatorio: lista, resultado -> {V,F,D}

somatorio([], 0).
somatorio([X|Y], R) :-
	somatorio(Y,G),
	R is X+G.


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado nao: Q -> {V,F}

nao(Q):- Q, !, fail.
nao(Q).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado quantosTem:A,Lista,Resultado  -> {V,F,D}

quantosTem(A,[],0).
quantosTem(A,[H|T],Resultado):-
	(A == H), quantosTem(A,T,R), Resultado is R+1.
quantosTem(A,[H|T],Resultado):-
	(A \= H), quantosTem(A,T,Resultado).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado pertence: X,L -> {V,F}

pertence(X,[]):- fail.
pertence(X,[X|T]):- X==X.
pertence(X,[H|T]):-
	X\=H,
	pertence(X,T).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado removeDup: L,R -> {V,F}

removeDup([],[]).
removeDup([X|T],R):-
	pertence(X,T),
	removeDup(T,R).
removeDup([X|T],[X|R]):-
	nao(pertence(X,T)),
	removeDup(T,R).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado ordenarDecresc: L,Resultado -> {V,F}

ordenarDecresc([X],[X]).
ordenarDecresc([X|Y],T):-
	ordenarDecresc(Y,R),
	insereOrdenado(X,R,T).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado insereOrdenado: X,L,Resultado  -> {V,F}

insereOrdenado((X1,Y1),[],[(X1,Y1)]).
insereOrdenado((X1,Y1),[(X2,Y2)|Z],[(X1,Y1)|[(X2,Y2)|Z]]):-
	Y1>Y2.
insereOrdenado((X1,Y1), [(X2,Y2)|Z], [(X2,Y2)|R2]) :-
	Y1=<Y2,
	insereOrdenado((X1,Y1),Z,R2).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado concatena(L1,L2,L3)->{V,F}
concatena([],L2,L2).
concatena([X|L1],L2,[X|L]) :-
	concatena(L1,L2,L).