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

%%https://stackoverflow.com/questions/19798844/finding-the-max-in-a-list-prolog
my_max([], R, Count, Count). %end
my_max([X|Xs], WK, Count, R):- X #>  WK, my_max(Xs, X, Count+1, R). %WK is Carry about
my_max([X|Xs], WK, Count, R):- X #=< WK, my_max(Xs, WK, Count, R).
my_max([X|Xs], R):- my_max(Xs, X, 1, R). %start


num_towers(X,W) :-
	my_max(X,C) , 
	W is C .  

check_side([],[]) .
check_side([Row|Rest],[Constrain|Tail]) :- 
	num_towers(Row,Constrain),
	check_side(Rest,Tail) .

reverse_grid([], Rev, Rev).
reverse_grid([Hd|Tl], Rev, X) :-
	reverse(Hd, Dh), append(Rev,[Dh], NList) ,reverse_grid(Tl, NList, X).
reverse_grid([Hd|Tl], X) :-
	reverse(Hd, Dh), reverse_grid(Tl, [Dh], X).


count_to_list([],L,L).
count_to_list(counts(A,B,C,D),L) :-
	count_to_list([],[A,B,C,D],L).


check_grid(T,Ts,C) :-
	nth(1,C,Top),
	check_side(Ts,Top),
	nth(3,C,Left),
	check_side(T,Left),
	reverse_grid(T,Rev_T),
	reverse_grid(Ts,Rev_Ts), 
	nth(2,C,Bottom),
	check_side(Rev_Ts,Bottom),
	nth(4,C,Right),
	check_side(Rev_T,Right).

tower(N,T,C) :- 
	length(T,N), %%total number of rows ic correct
	maplist(check_len(N),T), %%size of each row is correct
	maplist(applier(N),T) ,
	transpose(T,Ts),
	maplist(check_len(N),Ts),
	maplist(fd_all_different,T),
    maplist(fd_all_different,Ts),
    count_to_list(C,New),
    check_grid(T,Ts,New),
	maplist(labeler,T) . %%size of each column is correct

do_list(N, L):- 
  findall(Num, between(1, N, Num), L).

plain_tower(N,T,C) :- 
	length(T,N), %%total number of rows ic correct
	maplist(check_len(N),T), %%size of each row is correct
	do_list(N,Perm),
	forall(member(I,T), permutation(I,Perm)),
	transpose(T,Ts),
	maplist(check_len(N),Ts),
	forall(member(Is,Ts), permutation(Is,Perm)),
    count_to_list(C,New),
    check_grid(T,Ts,New). %%size of each column is correct


speedup(R) :- 
	statistics(cpu_time,[Start|_]), 
	plain_tower(4,[[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]] ,C) ,
	statistics(cpu_time,[Stop|_]),
	Plaintower is Stop-Start,
	statistics(cpu_time,[Go|_]), 
	tower(4,[[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]] ,C) ,
	statistics(cpu_time,[End|_]),
	Tower is Go-End, 
	R is 3/2 . 


 ambiguous(N, C, T1, T2) :- 
 	tower(N,T1,C), 
 	T1\=T2,
 	tower(N,T2,C). 





