%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inteligência Artificial MIEI /3  LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Prático Green Distribution

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Globais


:- style_check(-singleton).
:- dynamic cliente/2.
:- dynamic cliente/1.
:- dynamic estafeta/4.
:- dynamic estafeta/1.
:- dynamic encomenda/8.
:- dynamic encomenda/1.
:- dynamic entrega/8.
:- dynamic entrega/1.
:- dynamic client_count/3.
:- dynamic client_count/1.
:- dynamic estafeta_count/3.
:- dynamic estafeta_count/1.
:- dynamic encomenda_count/3.
:- dynamic encomenda_count/1.
:- dynamic entrega_count/3.
:- dynamic entrega_count/1.

% Call this ont the terminal to see all the results on the list: set_prolog_flag(answer_write_options,[max_depth(0)]).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% -------------------------------- Base de Conhecimento ------------------------------------

% --------------------------------    Clientes    ------------------------------------------

% Extensão do predicado cliente: Id, Nome -> {V,F}

% cliente(Id, Nome).

cliente( 1, tropa).
cliente( 2, bino).
cliente( 3, chico).
cliente( 4, paulo).
cliente( 5, andre).
cliente( 6, maria).
cliente( 7, jose).
cliente( 8, ãntonio).
cliente( 9, ana).
cliente( 10, raimundo).
cliente( 11, francisco).
cliente( 12, paulo).
cliente( 13, manuel).
cliente( 14, lucas).
cliente( 15, luis).
cliente( 16, francisca).
cliente( 17, daniel).
cliente( 18, gabriel).
cliente( 19, bea).
cliente( 20, antonio).

% -------------------------------    Encomendas    -----------------------------------------

% Extensão do predicado encomenda: Id, Peso, Volume, Transporte, IdCliente, IdEstafeta, Preço, Prazo -> {V,F}

% encomenda(Id, Peso, Volume, Transporte, IdCliente, IdEstafeta, Preco, Prazo).

encomenda(1,4,10,bicicleta,1,1,100,1).
encomenda(2,5,12,bicicleta,2,2,60,1).
encomenda(3,3,14,bicicleta,3,2,24,3).
encomenda(4,2,9,bicicleta,4,3,45,4).
encomenda(5,1,11,bicicleta,5,3,60,5).
encomenda(6,4,16,bicicleta,2,1,37,2).
encomenda(7,10,20,mota,7,1,22,1).
encomenda(8,13,23,mota,3,2,49,6).
encomenda(9,16,26,mota,9,1,75,6).
encomenda(10,20,30,mota,10,5,23,4).
encomenda(11,11,22,mota,5,6,33,2).
encomenda(12,8,13,mota,12,3,88,5).
encomenda(13,55,40,carro,13,8,73,4).
encomenda(14,61,34,carro,14,7,90,1).
encomenda(15,42,40,carro,15,2,40,1).
encomenda(16,85,60,carro,15,1,34,6).
encomenda(17,73,55,carro,17,1,58,6).
encomenda(18,35,25,carro,18,4,71,4).
encomenda(19,47,40,carro,19,4,99,2).
encomenda(20,2,9,bicicleta,20,5,81,4).

% --------------------------------    Entregas     -----------------------------------------


% Extensão do predicado entrega: IdEntrega, Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia -> {V,F}

% entrega(IdEntrega, Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia).

entrega(1, "Rua Travessa do Falcão", "Canelas", data(18, 11, 2021), 1, 3, 2,20).
entrega(2, "Rua Gomes Ferreira", "Santo Tirso", data(7, 5, 2021), 2, 5, 1,10).
entrega(3, "Rua da Universidade", "Braga", data(18, 10, 2021), 3, 1, 5,5).
entrega(4, "Rua Caminho Velho dos Barreiros", "Madeira", data(7, 4, 2021), 4, 5, 1,15).
entrega(5, "Rua 25 Abril", "Agrela", data(10, 11, 2021), 5, 3, 2,12).
entrega(6, "Rua Comes Damião", "Lisboa", data(17, 3, 2021), 6, 2, 3,16).
entrega(7, "Rua do Velho", "Vila Nova de Famalicão", data(8, 1, 2021), 7, 2, 3,24).
entrega(8, "Rua Nova", "Guimaraes", data(8, 5, 2021), 8, 4, 6,5).
entrega(9, "Rua dos Caes", "Aves", data(28, 7, 2021), 9, 3, 3,7).
entrega(10, "Rua Agua Nova", "Agua Longa", data(17, 5, 2021), 10, 3, 1,12).
entrega(11, "Rua Escura", "Porto", data(18, 11, 2021), 11, 3, 2,2).
entrega(12, "Rua Gomes Ferreira", "Santo Tirso", data(3, 5, 2021), 12, 4, 2,1).
entrega(13, "Rua Gomes Ferreira", "Santo Tirso", data(23, 11, 2021), 13, 4, 3,16).
entrega(14, "Rua 25 Abril", "Agrela", data(6, 2, 2021), 14, 5, 1,17).
entrega(15, "Rua 25 Abril", "Agrela", data(9, 2, 2021), 15, 5, 1,27).
entrega(16, "Rua Comes Damião", "Lisboa", data(1, 1, 2021), 16, 2, 3,12).
entrega(17, "Rua Travessa do Falcão", "Canelas", data(9, 11, 2021), 17, 3, 3,21).
entrega(18, "Rua Comes Damião", "Lisboa", data(7, 12, 2021), 18, 1, 2,12).
entrega(19, "Rua Travessa do Falcão", "Canelas", data(1, 12, 2021), 19, 1, 2,20).
entrega(20, "Rua da Universidade", "Braga", data(26, 8, 2021), 20, 5, 1,14).

% --------------------------------   Estafetas     ----------------------------------------

% Extensão do predicado estafeta: Id, Nome, Classificacao, TotalEntregas -> {V,F}

% estafeta(Id, Nome, Classificacao, TotalEntregas).

estafeta( 1, duarte, 5.0, 20).
estafeta( 2, esquerdo, 0.0, 15).
estafeta( 3, pedro, 0.1, 10).
estafeta( 4, madeira, 0.2, 20).
estafeta( 5, ana, 3.4, 5).
estafeta( 6, maria, 4.1, 30).
estafeta( 7, antonio, 4.3, 27).
estafeta( 8, lucas, 4.7, 13).
estafeta( 9, margarida, 1.2, 16).
estafeta( 10, patricia, 1.5, 19).
estafeta( 11, filipa, 2.8, 32).
estafeta( 12, francisco, 3.8, 35).
estafeta( 13, vasco, 4.6, 17).
estafeta( 14, goncalo, 4.1, 10).
estafeta( 15, andre, 5.0, 20).
estafeta( 16, grimaldo, 3.7, 29).
estafeta( 17, rafa, 4.5, 19).
estafeta( 18, guedes, 1.3, 31).
estafeta( 19, felix, 1.2, 10).
estafeta( 20, silva, 4.7, 35).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensão do predicado transporte: Transporte, Peso, Velocidade -> {V,F}

transporte(Transporte, Peso, Velocidade).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensão do predicado data: Dia, Mês, Ano -> {V,F}

data(Dia, Mes, Ano).

% -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




% Funções Auxiliares 

% Extensão do predicado entrega_prazo:  IdEntrega -> {V,F}

entrega_prazo(IdEntrega) :- 
    entrega(IdEntrega, _, _, _, IdEncomenda, _, Tempo, _),
    encomenda(IdEncomenda, _, _, _, _, _, _, Prazo),
    Prazo @>= Tempo.


% Extensão do predicado penalizacao:  IdEntrega -> {V,F}

penalizacao(IdEntrega) :- not(entrega_prazo(IdEntrega)), entrega(IdEntrega, _, _, _, X, _, _,_), encomenda(X,_,_,_,_,Y,_,_), estafeta(Y,_,_,_,G), estafeta(Y,_,_,_,Novo), Novo is G-1.



% -----------------------------------------------------------------     Clientes     -------------------------------------------------------------------------------------------------------------------- 

% Predicado que devolve a lista de todos os clientes
% Extensão do predicado: getAllClients: Lista -> {V, F}

getAllClients(Lista):- findall((Id, Nome), cliente(Id, Nome), Lista).


% Predicado que insere um cliente 
% Extensão do predicado new_cliente: Nome -> {V, F}

client_count(21).
new_cliente(Nome) :- client_count(C), asserta(cliente(C, Nome)), retract(client_count(C)), asserta(client_count(C + 1)).


% Predicado que apaga um cliente
% Extensão do predicado delete_cliente: IdCliente -> {V, F}

delete_cliente(IdCliente) :- retract(cliente(IdCliente, _)).

% -----------------------------------------------------------------     Encomendas     ------------------------------------------------------------------------------------------------------------------ 

% Predicado que devolve a lista de todas as encomendas
% Extensão do predicado getAllEncomendas: Lista -> {V, F}

getAllEncomendas(Lista):- findall((Id, Peso, Volume, Transporte, IdCliente, IdEstafeta, Preco, Prazo), encomenda(Id, Peso, Volume, Transporte, IdCliente, IdEstafeta, Preco, Prazo), Lista).


% Predicado que insere uma encomenda 
% Extensão do predicado new_encomenda: Volume, Transporte, IdCliente, IdEstafeta, Preco, Prazo -> {V, F}

encomenda_count(21).
new_encomenda(Peso, Volume, Transporte, IdCliente, IdEstafeta, Preco, Prazo) :- encomenda_count(C),
    asserta(encomenda(C, Peso, Volume, Transporte, IdCliente, IdEstafeta, Preco, Prazo)), retract(encomenda_count(C)), asserta(encomenda_count(C + 1)).


% Predicado que apaga uma encomenda
% Extensão do predicado delete_encomenda: IdEncomenda -> {V, F}

delete_encomenda(IdEncomenda) :- retract(encomenda(IdEncomenda, _, _, _, _, _, _, _)).

% -----------------------------------------------------------------     Entrega     --------------------------------------------------------------------------------------------------------------------- 

% Predicado que devolve a lista de todas as entregas
% Extensão do predicado getAllEntregas: Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia -> {V, F}

getAllEntregas(Lista):- findall((IdEntrega, Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia), entrega(IdEntrega, Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia), Lista).

% Predicado que insere uma entrega
% Extensão do predicado new_entrega: Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia -> {V, F}

entrega_count(21).
new_entrega(Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia) :- entrega_count(C),
    asserta(entrega(C, Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia)), retract(entrega_count(C)), asserta(entrega_count(C + 1)).


% Predicado que apaga uma entrega
% Extensão do predicado delete_entrega: IdEntrega -> {V, F}

delete_entrega(IdEntrega) :- retract(entrega(IdEntrega, _, _, _, _, _, _, _)).


% -----------------------------------------------------------------     Estafetas     -------------------------------------------------------------------------------------------------------------------

% Predicado que devolve a lista de todos os estafetas
% Extensão do predicado getAllEstafetas: Lista -> {V, F}

getAllEstafetas(Lista):- findall((Id, Nome, Classificacao, TotalEntregas), estafeta(Id, Nome, Classificacao, TotalEntregas), Lista).


% Predicado que insere um estafeta
% Extensão do predicado new_estafeta: Nome -> {V, F}

estafeta_count(21).
new_estafeta(Nome) :- estafeta_count(C), asserta(estafeta(C, Nome, 0, 0)), retract(estafeta_count(C)), asserta(estafeta_count(C + 1)).

% Predicado que apaga um estafeta
% Extensão do predicado delete_estafeta: IdEstafeta -> {V, F}

delete_estafeta(IdEstafeta) :- retract(estafeta(IdEstafeta, _, _, _)).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 


% Função que compara se uma data está num dado intervalo de tempo

entre_datas(data(D0, M0, A0), data(D10, M10, A10), data(D1, M1, A1)) :- 
	(maior_ou_igual(A1, A0), menor_ou_igual(A1, A10));
	(maior_ou_igual(A1, A0), menor_ou_igual(A1, A10), maior_ou_igual(M1, M0), menor_ou_igual(M1, M10));
	(maior_ou_igual(A1, A0), menor_ou_igual(A1, A10), maior_ou_igual(M1, M0),
	 menor_ou_igual(M1, M10), maior_ou_igual(D1, D0), menor_ou_igual(D1, D10)).

maior_ou_igual(N1, N2) :- N1 @>= N2.
menor_ou_igual(N1, N2) :- N1 @=< N2.


% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 1:

estafeta_ecologico(IdEstafeta) :- findall((IdEstafeta, Distancia, Trans), 
    (entrega(_, _, _, _, IdEncomenda, _, _, Distancia), encomenda(IdEncomenda, _, _, Trans, _, IdEstafeta, _, _)),
    Lista),
    findEco(Lista, [], ResLista),
    sumRep(ResLista, [], ResResLista),
    smallest(ResResLista, (0, 100000000), (IdEstafeta, Score)), !.

smallest([], (TId, TM), (TId, TM)).
smallest([(Id, M) | T], (TId, TM), R) :- M @< TM, smallest(T, (Id, M), R).
smallest([(Id, M) | T], (TId, TM), R) :- M @>= TM, smallest(T, (TId, TM), R).

findEco([], N, N).
findEco([(IdEstafeta, Distancia, Trans) | T], N, R) :- findEco(T, [(IdEstafeta, M) | N], R), trans(Trans, TT),
    M is Distancia * TT.

sumRep([], N, N).
sumRep([(IdEstafeta, M) | T], N, R) :- contains(N, (IdEstafeta, M)), add(N, (IdEstafeta, M), [], NewN), sumRep(T, NewN, R).
sumRep([(IdEstafeta, M) | T], N, R) :- sumRep(T, [(IdEstafeta, M) | N], R).

contains([(Id, M) | T], (Id, NewM)).
contains([(Id, M) | T], (NewId, NewM)) :- contains(T, (NewId, NewM)).

add([], (Id, M), N, N).
add([(IdEstafeta, LM) | T], (IdEstafeta, M), N, R) :- add(T, (IdEstafeta, M), [(IdEstafeta, G) | N], R), G is LM + M.
add([(LId, LM) | T], (IdEstafeta, M), N, R) :- add(T, (IdEstafeta, M), [(LId, LM) | N], R).

trans(carro, 2).
trans(mota, 1).
trans(bicicleta, 0).

% -----------------------------------------------------------------------

% Pergunta 2:


% Predicado que devolve a lista dos ids dos estafetas que entregaram determinada(s) encomenda(s) a um determinado cliente
% Extensão do predicado getEstafetas: Lista1, Lista2, Resultado -> {V, F}


getEstafetas(IdCliente, [], []).
getEstafetas(IdCliente, [IdEncomenda|Tail1], [estafeta(IdEstafeta, Nome, Classificacao, TotalEntregas)|Tail2]):- encomenda(IdEncomenda, _, _, _, IdCliente, IdEstafeta, _, _), estafeta(IdEstafeta, Nome, Classificacao, TotalEntregas), getEstafetas(IdCliente, Tail1, Tail2). 

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 3:


% Predicado que devolve a lista de clientes servidos por um dado estafeta
% Extensão do predicado clientes_servidos:  IdEstafeta, Resultado -> {V,F}

clientes_servidos(IdEstafeta, Clientes) :-
	findall(IdCliente, (entrega(_, _, _, _, X, _, _, _), encomenda(X, _, _, _, IdCliente, IdEstafeta, _, _)), C),
	elimina_repetidos(C, Clientes).


% Predicado que elimina elementos repetidos de uma lista
% Extensão do predicado elimina_repetidos: Lista, Resultado -> {V, F}

elimina_repetidos(L, R) :- elimina_repetidos_aux(L, [], R).
elimina_repetidos_aux([], L, L).
elimina_repetidos_aux([X | T], L, R) :- member(X, T), elimina_repetidos_aux(T, L, R).
elimina_repetidos_aux([X | T], L, R) :- elimina_repetidos_aux(T, [X|L], R).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 4:


% Predicado que calcula o valor faturado pela Green Distribution num determinado dia 
% Extensão do predicado entregasDoDia: Data, Resultado -> {V, F} 

entregasDoDia(data(Dia, Mes, Ano), Total):- findall(IdEntrega, entrega(IdEntrega, _, _, data(Dia, Mes, Ano), _, _, _, _), Lista), somaPrecos(Lista, 0, Total).


% Predicado que dada uma lista de entregas entregas devolve o preço total das encomendas 
% Extensão do predicado somaPrecos: Lista, Acumulador, Total -> {V, F}

somaPrecos([], Total, Total).
somaPrecos([IdEntrega|Tail], Acum, Total):- entrega(IdEntrega, _, _, _, IdEncomenda, _, _, _), encomenda(IdEncomenda, _, _, _, _, _, Preco, _), NovoAcum is Acum + Preco, somaPrecos(Tail, NovoAcum, Total). 

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 5: 

% entrega(IdEntrega, Rua, Freguesia, Data, IdEncomenda, Classificacao, Tempo, Distancia).

% Predicado que devolve as zonas (Freguesias e Ruas) com maior volume de entregas
% Extensão do predicado maior_volume_order: Lista -> {V, F}

maior_volume_freg(L) :-
	findall(Freg, entrega(_, _, Freg, _, _, _, _, _), Lista),
	more_freq(Lista, [], L), !.


% Predicado que devolve as zonas (Ruas) com maior volume de entregas
% Extensão do predicado maior_volume_rua: Lista -> {V, F}

maior_volume_rua(L) :-
	findall(Rua, entrega(_, Rua, _, _, _, _, _, _), Lista),
	more_freq(Lista, [], L), !.


% Predicado que devolve uma lista de pares, no qual cada par é constituído por (Elemento, Quantidade)
% Extensão do predicado more_freq: Lista1, Lista2, Resultado -> {V, F}

more_freq([], L, L).
more_freq([H | T], L, R) :- contar(H, [H | T], N), delete(H, T, [], NT), more_freq(NT, [(H, N)|L], R).


% Predicado que conta a quantidade de vezes que um elemento aparece numa lista
% Extensão do predicado contar: Elemento, Lista, Quantidade -> {V, F}

contar(H, [], 0).
contar(H, [H | T], N) :- contar(H, T, G), N is G + 1.
contar(H, [X | T], N) :- contar(H, T, N).


% Predicado que remove todas aas ocorrências de um elemento de uma lista
% Extensão do predicado delete: Elemento, Lista, Resultado -> {V, F}
delete(H, [], N, N).
delete(H, [H | T], P, N) :- delete(H, T, P, N).
delete(H, [X | T], P, N) :- delete(H, T, [X | P], N).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 6:


% Predicado que devolve a classificação média de satisfação de cliente para um determinado estafeta
% Extensão do predicado classificacao_estafeta: IdEstafeta, Media -> {V, F}

classificacao_estafeta(IdEst,Med) :-  findall(Class, (entrega(_,_,_,_,X,Class, _, _),encomenda(_,_,_,_,_,IdEst,_, _)),T) , length(T,L) ,soma(T,S), Med is S/L .


% Predicado que devolve a soma dos elementos de uma lista
% Extensão do predicado soma: Lista, Total -> {V, F}

soma([],0).
soma([H|T],S):-soma(T,G),S is H+G.

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 7:

% Predicado que devolve o número total de entregas por cada meio de transporte num intervalo de tempo
% Extensão do predicado entregas_por_transporte_intervalo: DataInicio, DataFim, Resultado1, Resultado2, Resultado3 -> {V, F}

entregas_por_transporte_intervalo(Data0, Data10, Bicicleta, Mota, Carro) :- 
	findall(IdEntregaBic, (entrega(IdEntregaBic, _, _, Data1, X, _, _, _),
		encomenda(X, _, _, bicicleta, _, _, _, _), entre_datas(Data0, Data10, Data1)), ListaBic),
	length(ListaBic, Bicicleta),

	findall(IdEntregaMot, (entrega(IdEntregaMot, _, _, Data2, X, _, _, _),
	 	encomenda(X, _, _, mota, _, _, _, _), entre_datas(Data0, Data10, Data2)), ListaMot),
	length(ListaMot, Mota),

	findall(IdEntregaCar, (entrega(IdEntregaCar, _, _, Data3, X, _, _, _),
	 	encomenda(X, _, _, carro, _, _, _, _), entre_datas(Data0, Data10, Data3)), ListaCar),
	length(ListaCar, Carro).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 8:


% Predicado que calcula o número total de entregas pelos estafetas num determinado intervalo de tempo
% Extensão do predicado numero_total_entregas_intervalo: DataÍnicio, DataFim, Total -> {V, F}

numero_total_entregas_intervalo(DataInicio, DataFim, Total):- todas_entregas(DataInicio, DataFim, EntregasTotais), length(EntregasTotais, Total).


% Predicado que devolve todas as entregas feitas num intervalo de tempo
% Extensão do predicado todas_entregas: DataÍnicio, DataFim, Resultado -> {V, F}

todas_entregas(DataInicio, DataFim, Lista):- findall(X, (entrega(X, _,_,Data,_,_, _, _), entre_datas(DataInicio, DataFim, Data)), Lista).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 9:


% Predicado que devolve o número de encomendas entregues (E) e não entregues (N) num determinado intervalo de tempo
% Extensão do predicado encomendas_tempo: DataÍnicio, DataFim, Total1, Total2 -> {V, F}

encomendas_tempo(Inicio,Fim, E, N) :- 
   findall(IdEncomenda, (entrega(_,_,_,D,IdEncomenda,_, _, _),entre_datas(Inicio,Fim, D)),Lista),
   length(Lista, E),
   findall(IdEnc,(encomenda(IdEnc,_,_,_,_,_,_, _), not(member(IdEnc, Lista))), R),
   length(R, N).

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pergunta 10:


% Predicado que calcula o peso total transportado por um estafeta num determinado dia
% Extensão do predicado peso_por_estafeta_em: Data, IdEstafeta, Total -> {V, F}

peso_por_estafeta_em(data(D, M, A), IdEstafeta, Total) :-
	findall(Peso, (entrega(_, _, _, _, X, _, _, _), encomenda(X, Peso, _, _, _, IdEstafeta, _, _)), Pesos),
	sum(Pesos, Total).

sum([], 0).
sum([H | T], N) :- sum(T, G), N is H + G.

% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
