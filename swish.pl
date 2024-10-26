%———————————————————————————————————————————————
% Learning from family relations

%  Background knowledge

% backliteral(+p(V1,…,Vn),[V1,…,Vn])
% says that the literal of the form p(V1,…,Vn), with variables
% V1,…,Vn possibly renamed, are part of the hypothesis language.
%------------------------

%backliteral(parent(X,Y),[X,Y]).
%backliteral(male(X),[X]).
%backliteral(female(X),[X]).

%prolog_predicate(parent(_,_)).
%prolog_predicate(male(_)).
%prolog_predicate(female(_)).

%———————————————————————————————————————————————
parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
parent(pat, eve).

male(tom).
male(bob).
male(jim).
female(pam).
female(liz).
female(ann).
female(pat).
female(eve).


% has_daughter(X) <- parent(X,Y) ^ female(Y).

%———————————————————————————————————————————————
% Positive examples
% ex(+Example): +Example is a positive example
ex(has_daughter(tom)). %
ex(has_daughter(bob)).
ex(has_daughter(pat)).

%———————————————————————————————————————————————
% Negative examples
%nex(+Example): +Example is a 
nex(has_daughter(pam)).
nex(has_daughter(jim)).

start_hyp([[has_daughter(X)]/[X]]).

%not(P):-
%    P, !, fail.
%not(_).




%------------NAO SEI------------------------------




%———————————————————————————————————————————————
% Learning from family relations
% prove(Goal, Hypo, Ans)
%   Ans = yes …

prove(Goal, Hypo, Answer):-
    max_proof_length(D),
    prove(Goal, Hypo, D, RestD),
    (RestD >= 0, Answer = yes		% Proved
     ;				      
     RestD < 0, Answer = maybe).	% Maybe, but it looks like inf. loop
prove(Goal, _, no).			% Otherwise goal definitely cannot be proved


%———————————————————————————————————————————————
% prove(Goal, Hypo, MaxD, RestD)

prove(G, H, D, D):-
    D <0, !.
prove([], _, D, D):- !.
prove([G1|Gs],Hypo,D0,D):-
    prove(G1,Hypo,D0,D1),
    prove(Gs,Hypo,D1,D).
prove(G,_,D,D):-
    prolog_predicate(G),
    call(G).
prove(G,Hypo,D0,D):-
    D0 =< 0, !,
    D is D0-1
    ;
    D1 is D0 - 1,
    member(Clause/Vars, Hypo),
    copy_term(Clause,[Head|Body]),
    G = Head,
    prove(Body, Hypo,D1,D).

%——————————————————————————————————————————————-——————————————-
induce(Hyp):-
    iter_deep(Hyp,0).

iter_deep(Hyp,MaxD):-
    write('MaxD= '), write(MaxD), nl,
    start_hyp(Hyp0),
    complete(Hyp0),
    depth_first(Hyp0,Hyp,MaxD)
    ;
    NewMaxD is MaxD+1,
    iter_deep(Hyp, NewMaxD).

depth_first(Hyp,Hyp,_):-
    consistent(Hyp).
depth_first(Hyp0,Hyp,MaxD0):-
    MaxD0 > 0,
    MaxD1 is MaxD0-1,
    refine_hyp(Hyp0, Hyp1),
    complete(Hyp1),
    depth_first(Hyp1,Hyp,MaxD1).

complete(Hyp):-
    not(ex(E),				% A positive example
        once(prove(E, Hyp, Answer)),	% Prove it with Hyp
        Answer \== yes).		% possibly provable

consistent(Hyp):-
    not(nex(E),				% A negative example
        once(prove(E, Hyp, Answer)),	% Prove it with Hyp
        Answer \== no).			% possibly provable

refine_hyp(Hyp0,Hyp):-
    conc(Clauses1,[Clause0/Vars0 | Clauses2], Hyp0),
    conc(Clauses1,[Clause/Vars | Clauses2], Hyp),
    refine(Clause0, Vars0, Clause, Vars).

refine(Clause, Args, Clause, NewArgs):-
    conc(Args1, [A | Args2], Args),
    member(A, Args2),
    conc(Args1, Args2, NewArgs).
refine(Clause,Args,NewClause, NewArgs):-
    length(Clause, L),
    max_clause_length(MaxL),
    L < MaxL,
    backliteral(Lit, Vars),
    conc(Clause,[Lit],NewClause),
    conc(Args, Vars, NewArgs).

max_proof_length(10).

max_clause_length(3).

conc([],L,L).
conc([X|T],L,[X|L1]):-
    conc(T,L,L1).

%———————————————————————————————————————————————
not(A,B,C):-
    A,
    B,
    C, !, fail.
not(_,_,_).

% learning 2 predicate 
% odd and even
even([]).
even(A,B|C):-
    even(C).
odd([A|B]):-
    even(B).
% introducing odd and even length for lists
backliteral(even(L),[L:list],[]).
backliteral(odd(L),[L:list],[]).

term(list,[X|L],[X:item,L:list]).
term(list,[],[]).

prolog_predicate(fail).

start_clause([odd(L)/[L:list]]).
start_clause([even(L)/[L:list]]).
