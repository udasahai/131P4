%%https://stackoverflow.com/questions/30800407/prolog-iterating-through-list
%%http://www.swi-prolog.org/pldoc/man?predicate=transpose/2

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).




unique([]).
unique([X|Xs]) :- \+ memberchk(X, Xs), unique(Xs).

tower(N,T,C) :- 
	%% forall(member(A,T),fd_domain(A,1,N)) ,
	%% forall(member(A,T),fd_labeling(A)) ,
	length(T,N), %%total number of rows ic correct
	forall(member(A,T),length(A,N)) , %%size of each row is correct
	forall(member(A,T),unique(A)) , %%make sure that the rows contain all unique elements.
	transpose(T,Ts) , %% rows -> columns 
	forall(member(A,Ts),length(A,N)) , %%size of each column is correct
	forall(member(A,Ts),unique(A)) ,
	forall(member(A,T),fd_domain(A,1,N)) ,
	forall(member(A,T),fd_labeling(A)) .







%% goal(L1,X) :- 
%% 	member(X,L1) . 		

%% lister(L) :- 
%% 	maplist(goal(L),L) .


%% tower(5,
%%          [[2,3,4,5,1],
%%           [5,4,1,3,2],
%%           [4,1,5,2,3],
%%           [1,2,3,4,5],
%%           [3,5,2,1,4]],
%%          C).