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

applier(N,X) :- 
	fd_domain(X,1,N) .

labeler(X) :-
	fd_labeling(X) .

check_len(N,X) :-
	length(X,N) . 


unique([]).
unique([X|Xs]) :- \+ memberchk(X, Xs), unique(Xs).

tower(N,T,C) :- 
	length(T,N), %%total number of rows ic correct
	transpose(T,Ts),
	maplist(check_len(N),T), %%size of each row is correct
	maplist(check_len(N),Ts),
	maplist(fd_all_different,T),
    maplist(fd_all_different,Ts),
	maplist(applier(N),T) ,
	maplist(labeler,T) . %%size of each column is correct








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



