%T1
%Autores:
%Alexandre Regali Seleghim (RA 551473)
%Pedro de Oliveira Zani (RA 408565)

programa:-
write('Digite a lista 1: '),
read(Lista1),
nl,
write('Digite a lista 2: '),
read(Lista2),
nl,
monta_pares(Lista1,Lista2,_).

%listas do exemplo1:
%[a, b, Z, [a, x], [5,x], [x], par(c,d)].
%[4.6, 5, w, [], F, b, [], par(c,d), [], par(1,2)].

%listas do exemplo2:
%[a, b, [d], [j, y], c].
%[j, k, [[c]], b, j, a, j].

%funcao desparentize
%L1, Lout
desparentize([], []).
desparentize([X|Y], [X|Z]) :- not(is_list(X)), desparentize(Y,Z), !.
desparentize([X|Y], [X|Z]) :- is_list(X), ehVazia(X), desparentize(Y,Z), !.
desparentize([X|Y], Z) :- is_list(X), not(ehVazia(X)), desparentize(X,X1), desparentize(Y,Y1), append(X1, Y1, Z), !.

%funcao de tirar, de L1, os elementos comuns entre L1 e L2
%L1, L2, Lout
tira_comuns([], _, []).
tira_comuns([X1|Y1], L2, [X1|Z]) :- not(member(X1, L2)), tira_comuns(Y1, L2, Z).
tira_comuns([_|Y1], L2, Z) :- tira_comuns(Y1, L2, Z).


%funcao de contar as ocorrencias de um elemento numa dada lista L1
%Elemento, L1, N
conta(_, [], 0).
conta(X, [X | T], N) :- !, conta(X, T, N1), N is N1 + 1.
conta(X, [_ | T], N) :- conta(X, T, N).


%funcao de remover todas as ocorrencias de um elemento numa lista L1
%Elemento, L1, Lout
remove_elemento(_, [], []) :- !.
remove_elemento(X, [X|Y], Z) :- !, remove_elemento(X, Y, Z).
remove_elemento(X, [T|Y], Z) :- !, remove_elemento(X, Y, Y2), append([T], Y2, Z).

%funcao de remover todas as variaveis de uma lista L1
%L1, Lout
remove_variaveis([], []).
remove_variaveis([X|Y], [X|Z]) :- not(var(X)), remove_variaveis(Y,Z), !.
remove_variaveis([X|Y], Z) :- var(X), remove_variaveis(Y,Z), !.

%calcula o tamanho da lista L1
%L1
tamanho([],0).
tamanho([_|T],N) :- tamanho(T,X), N is X+1.

%verifica se a lista L1 eh vazia
%L1
ehVazia(L) :- tamanho(L,X),X=:=0.

%funcao que constroi a lista final com o par [elemento, numero de ocorrencias] da lista L1
%L1, Lout
final([], Lout) :- length([], J), J =:= 0, write(Lout), !.
final([X|Y], Lout) :-
conta(X,[X|Y],N),
append(Lout,[[X,N]],Laux),
remove_elemento(X, [X|Y], Lout2),
final(Lout2, Laux).

%funcao que trata as duas listas e depois chama a funcao final
monta_pares(A,B,Lout) :-
desparentize(A, A2),
desparentize(B, B2),
remove_variaveis(A2, A3),
remove_variaveis(B2, B3),
tira_comuns(A3,B3, A4),
tira_comuns(B3,A3, B4),
append(A4,B4, L3),
final(L3, Lout).
