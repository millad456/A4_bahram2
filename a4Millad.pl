:-style_check(-discontiguous).
:-style_check(-singleton).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(clpb)).
:- use_module(library(clpfd)).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Context for typing environments
context([]).

extend_context(Context, Var, Type, [(Var, Type)|Context]) :-
    atom(Var), type(Type), context(Context).

lookup_context([(Var, Type)|_], Var, Type).
lookup_context([_|Tail], Var, Type) :-
    lookup_context(Tail, Var, Type).

% Type: expr
expr(var(V)) :- atom(V). %% atomic variable
expr(lam(X,T,E)) :- atom(X), type(T), expr(E). %% lambda 
expr(app(X,Y)) :- expr(X), expr(Y). %% app
expr(pair(X,Y)) :- expr(X), expr(Y). %% pair
expr(fst(X)) :- expr(X). %% first
expr(snd(X)) :- expr(X). %% second
expr(true_). %% boolean's true and false
expr(false_).
expr(and(X,Y)) :- expr(X), expr(Y). %% and
expr(if_then_else(B,T,E)) :- expr(B), expr(T), expr(E). %% if Bool, then T, else E
%expr(let(V, E1, E2)) :- expr(V), expr(E1), expr(E2). %% letin //this doesn't work and is outdated
expr(let(X, E1, E2)) :- atom(X), expr(E1), expr(E2).

% Type: value
value(lam(_, _, _)).
value(bool(_)).
value(pair(V1, V2)) :- 
    value(V1), 
    value(V2).

% Define types
type(bool).
type(nat).  % Assuming natural numbers are part of the language
type(func(T1, T2)) :- type(T1), type(T2).
type(pair(T1, T2)) :- type(T1), type(T2).

%the substitution function, taken from an earilier tutorial
subst(X, V, X, V) :- !.
subst(_, _, Y, Y) :- var(Y), !.
subst(X, V, app(E1, E2), app(SubE1, SubE2)) :-
    subst(X, V, E1, SubE1),
    subst(X, V, E2, SubE2).
subst(X, _, lam(X, T, E), lam(X, T, E)) :- !.
subst(X, V, lam(Y, T, E), lam(Y, T, SubE)) :-
    X \= Y, 
    subst(X, V, E, SubE).
subst(X, V, let(Y, E1, E2), let(Y, SubE1, SubE2)) :-
    subst(X, V, E1, SubE1),
    subst(X, V, E2, SubE2).
subst(X, V, pair(E1, E2), pair(SubE1, SubE2)) :-
    subst(X, V, E1, SubE1),
    subst(X, V, E2, SubE2).
subst(X, V, fst(E), fst(SubE)) :-
    subst(X, V, E, SubE).
subst(X, V, snd(E), snd(SubE)) :-
    subst(X, V, E, SubE).
subst(X, V, and(E1, E2), and(SubE1, SubE2)) :-
    subst(X, V, E1, SubE1),
    subst(X, V, E2, SubE2).
subst(X, V, if_then_else(E1, E2, E3), if_then_else(SubE1, SubE2, SubE3)) :-
    subst(X, V, E1, SubE1),
    subst(X, V, E2, SubE2),
    subst(X, V, E3, SubE3).
subst(_, _, Term, Term).

% Application of a function to a value (beta-reduction)
sstep(app(lam(X, _, E1), V2), E1Sub) :-
    value(V2),
    subst(X, V2, E1, E1Sub).
% Application where the first part of the expression can be further reduced. E1 -> E1', E1, E2 -> E1', E2
sstep(app(E1, E2), app(E1Sub, E2)) :-
    sstep(E1, E1Sub).
% Application where the first part is a value and the second part can be reduced. E2 -> E2', V1, E2 -> V1, E2'
sstep(app(V1, E2), app(V1, ESub2)) :-
    value(V1),
    sstep(E2, ESub2).
% First part of a pair. fst(V1, V2) -> V1
sstep(fst(pair(V1, _)), V1) :-
    value(V1).
% First part of a pair where the pair itself can be further reduced. 
sstep(fst(E), fst(ESub)) :-
    sstep(E, ESub).
% Second part of a pair. snd(V1, V2) -> V2
sstep(snd(pair(_, V2)), V2) :-
    value(V2).
% Second part of a pair where the pair itself can be further reduced
sstep(snd(E), snd(ESub)) :-
    sstep(E, ESub).
% Logical AND where the first expression is true
sstep(and(true, E2), E2).
% Logical AND where the first expression is false
sstep(and(false, _), false).
% Logical AND where the first expression can be further reduced
sstep(and(E1, E2), and(E1Sub, E2)) :-
    sstep(E1, E1Sub).
% If-then-else construct where the condition is true. if true then E2, else E3 -> E2
sstep(if_then_else(true, E2, _), E2).
% If-then-else construct where the condition is false. if false then E2, else E3 -> E3
sstep(if_then_else(false, _, E3), E3).
% If-then-else construct where the condition can be further reduced. E1 -> E1' if true then E2, else E3 -> E2
sstep(if_then_else(E1, E2, E3), if_then_else(E1Sub, E2, E3)) :-
    sstep(E1, E1Sub).
% Let expressions 
sstep(let(X, E1, E2), ESub) :-
    sstep(E1, E1Sub),
    subst(X, E1Sub, E2, ESub).
% Let expressions where the bound expression is a value
sstep(let(X, V1, E2), E2Sub) :-
    value(V1),
    subst(X, V1, E2, E2Sub). 
%rule added by me 
sstep(pair(E1, E2), pair(ESub1, ESub2)) :-
    write('Trying to reduce pair...\n'),
    sstep(E1, ESub1),
    sstep(E2, ESub2),
    write('Reduced pair!\n'). 


% Value definitions
value(num(_)).   % Assuming num(N) is a value for numbers
value(bool(_)).  % true and false are already values
value(lam(_, _, _)). % Lambda abstraction is a value
% ... include other value definitions as necessary

typed(Context, true_, bool).
typed(Context, false_, bool).
typed(Context, var(X), Type) :- lookup_context(Context, X, Type).
typed(Context, lam(X, T, E), arrow(T, ExprType)) :-
    extend_context(Context, X, T, ExtendedContext),
    typed(ExtendedContext, E, ExprType).
typed(Context, app(X, Y), T) :-
    typed(Context, X, arrow(T1, T)),
    typed(Context, Y, T1).
typed(Context, pair(E1, E2), pair(T1, T2)) :-
    typed(Context, E1, T1),
    typed(Context, E2, T2).
typed(Context, fst(E), T) :-
    typed(Context, E, pair(T, _)).
typed(Context, snd(E), T) :-
    typed(Context, E, pair(_, T)).
typed(Context, and(X, Y), bool) :-
    typed(Context, X, bool),
    typed(Context, Y, bool).
typed(Context, if_then_else(X, Y, Z), T) :-
    typed(Context, X, bool),
    typed(Context, Y, T),
    typed(Context, Z, T).
typed(Context, let(X, E1, E2), T) :-
    typed(Context, E1, TypeE1),
    extend_context(Context, X, TypeE1, ExtendedContext), % Extend the context with the type of E1
    typed(ExtendedContext, E2, T).
% Additional type rules for other constructs can be added following the same pattern.


% If X is a value, then it is already fully evaluated.
mstep(X, X) :- value(X).
% If X is not a value, it needs to be evaluated step by step until it becomes a value.
mstep(X, Y) :-
    sstep(X, Z),  % Take a single step from X to some intermediate expression Z.
    mstep(Z, Y).  % Recursively evaluate Z further until it becomes a value Y.

% Additional rules for other constructs
% Assuming 'and', 'fst', 'snd', 'let', lambda application, etc. are part of your language

% Logical AND where the first expression can be further reduced
tsstep(and(E1, E2), and(E1Sub, E2), e_And(T)) :- tsstep(E1, E1Sub, T).
% Logical AND where the first expression is true
tsstep(and(true_, E2), E2, e_AndTrue).
% Logical AND where the first expression is false
tsstep(and(false_, _), false_, e_AndFalse).
% First part of a pair where the pair itself can be further reduced
tsstep(fst(E), fst(ESub), e_Fst(T)) :- tsstep(E, ESub, T).
% Second part of a pair where the pair itself can be further reduced
tsstep(snd(E), snd(ESub), e_Snd(T)) :- tsstep(E, ESub, T).
% Application of a function to a value
tsstep(app(lam(X, T, E1), V2), E1Sub, e_Apply(T)) :-
    value(V2),
    subst(X, T, V2, E1, E1Sub, T).
% Application where the first part of the expression can be further reduced
tsstep(app(E1, E2), app(E1Sub, E2), e_App(T)) :-
    tsstep(E1, E1Sub, T).
% Let expressions
tsstep(let(X, E1, E2), let(X, E1Sub, E2), e_Let(T)) :-
    tsstep(E1, E1Sub, T).
% Let expressions where the bound expression is a value
tsstep(let(X, V1, E2), E2Sub, e_LetValue(T)) :-
    value(V1),
    subst(X, V1, E2, E2Sub, T).



%This part is untested because the functions it relies on don't really work. 
% Type derivation for boolean literals
typederiv(bool(true), bool, t_BoolTrue).
typederiv(bool(false), bool, t_BoolFalse).

% Type derivation for if_then_else expressions
typederiv(if_then_else(X, Y, Z), T, t_If(XD, YD, ZD)) :-
  typederiv(X, bool, XD),
  typederiv(Y, T, YD),
  typederiv(Z, T, ZD).
% Type derivation for lambda expressions (functions)
typederiv(lam(X, T, E), arrow(T1, T2), t_Lam(XD,T, ED)) :-
  typederiv(X, T, T1, XD),
  typederiv(E, T, T2, ED).
% Type derivation for function applications
typederiv(app(E1, E2), T, t_App(ED1, ED2)) :-
  typederiv(E1, arrow(T2, T), ED1),
  typederiv(E2, T2, ED2).
% Type derivation for pairs
typederiv(pair(E1, E2), pair(T1, T2), t_Pair(ED1, ED2)) :-
  typederiv(E1, T1, ED1),
  typederiv(E2, T2, ED2).
% Type derivation for the first element of a pair
typederiv(fst(E), T, t_Fst(ED)) :-
  typederiv(E, pair(T, _), ED).
% Type derivation for the second element of a pair
typederiv(snd(E), T, t_Snd(ED)) :-
  typederiv(E, pair(_, T), ED).
% Type derivation for logical AND expressions
typederiv(and(E1, E2), bool, t_And(ED1, ED2)) :-
  typederiv(E1, bool, ED1),
  typederiv(E2, bool, ED2).
% Type derivation for let expressions
typederiv(let(_, E1, E2), T, t_Let(XD, ED1, ED2)) :-
  typederiv(E1, T1, ED1),
  % Here we assume X is of type T1 within E2
  typederiv(E2, T, ED2).

% Proving typed values (typedvalue predicate)
typedvalue(V, T) :- value(V), typederiv(V, T, _).

%run tests
/*
run_tests :- 
    consult('testing.pl'),
    run_all_tests.
*/

