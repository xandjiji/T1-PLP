%T1
%Autores:
%Alexandre Regali Seleghim (RA 551473)
%Thiago Bretas de Souza (RA 551899)


programa:-
write('Digite a lista 1: '),
read(Lista1),
nl,
write('Digite a lista 2: '),
read(Lista2),
nl,
conta_atomos(Lista1,Lista2,Lout).





%L1, Lout
desparentize([], []).
desparentize([X|Y], [X|Z]) :- not(is_list(X)), desparentize(Y,Z), !.
desparentize([X|Y], Z) :- desparentize(X, X1), desparentize(Y, Y1), append(X1, Y1, Z).


%L1, L2, Lout
tira_nao_comuns([], _, []).
tira_nao_comuns([X1|Y1], L2, [X1|Z]) :- member(X1, L2), tira_nao_comuns(Y1, L2, Z).
tira_nao_comuns([_|Y1], L2, Z) :- tira_nao_comuns(Y1, L2, Z).


%Elemento, L1, N
conta(_, [], 0).
conta(X, [X | T], N) :- !, conta(X, T, N1), N is N1 + 1.
conta(X, [_ | T], N) :- conta(X, T, N).


%Elemento, L1, Lout
remove_elemento(_, [], []) :- !.
remove_elemento(X, [X|Y], Z) :- !, remove_elemento(X, Y, Z).
remove_elemento(X, [T|Y], Z) :- !, remove_elemento(X, Y, Y2), append([T], Y2, Z).


final([], Lout) :- length([], J), J =:= 0, write(Lout), !.
final([X|Y], Lout) :-
conta(X,[X|Y],N),
append(Lout,[[X,N]],Laux),
remove_elemento(X, [X|Y], Lout2),
final(Lout2, Laux).
	
	
	
conta_atomos([],[],[]).
conta_atomos(L1,L2,Lout):-
desparentize(L1, L11),
desparentize(L2, L22),
tira_nao_comuns(L11,L22, L111),
tira_nao_comuns(L22,L11, L222),
append(L111,L222, L3),
final(L3, Lout).