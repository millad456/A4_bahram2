

%function to rul all tests
run_all_tests :-
      run_expr_tests,
      run_value_tests,
      run_type_tests,
      run_subst_tests,
      run_typed_tests,
      run_typed_context_tests,
      run_sstep_tests,
      run_typederiv_tests,
      run_mstep_tests.
  
  run_expr_tests :-
      run_test(test_expr1),
      run_test(test_expr2),
      run_test(test_expr3),
      run_test(test_expr4),
      run_test(test_expr5),
      run_test(test_expr6),
      run_test(test_expr7),
      run_test(test_expr8),
      run_test(test_expr9).
  
  run_value_tests :-
      run_test(test_value1),
      run_test(test_value2),
      run_test(test_value3).
  
  run_type_tests :-
      run_test(test_type1),
      run_test(test_type2),
      run_test(test_type3),
      run_test(test_type4).
  
  run_subst_tests :-
      run_test(test_subst1),
      run_test(test_subst2),
      run_test(test_subst3),
      run_test(test_subst4),
      run_test(test_subst5).
  
  run_typed_tests :-
      run_test(test_typed1),
      run_test(test_typed2),
      run_test(test_typed3),
      run_test(test_typed4),
      run_test(test_typed5),
      run_test(test_typed6),
      run_test(test_typed7),
      run_test(test_typed8),
      run_test(test_typed9),
      run_test(test_typed10),
      run_test(test_typed11).
  
  run_typed_context_tests :-
      run_test(test_typed_context1),
      run_test(test_typed_context2),
      run_test(test_typed_context3).
  
  run_sstep_tests :-
      consult('a4Millad.pl'),
      run_test(test_sstep1),
      run_test(test_sstep2),
      run_test(test_sstep3),
      run_test(test_sstep4),
      run_test(test_sstep5),
      run_test(test_sstep6),
      run_test(test_sstep7),
      run_test(test_sstep8),
      run_test(test_sstep9),
      run_test(test_sstep10),
      run_test(test_sstep11),
      run_test(test_sstep12).
  
  run_mstep_tests :-
      run_test(test_mstep1).
  
  run_typederiv_tests :-
    run_test(test_typederiv1),
    run_test(test_typederiv2),
    run_test(test_typederiv3),
    run_test(test_typederiv4),
    run_test(test_typederiv5),
    run_test(test_typederiv6),
    run_test(test_typederiv7),
    run_test(test_typederiv8),
    run_test(test_typederiv9),
    run_test(test_typederiv10).

%for running tests, we define it ourselves
  run_test(Test) :-
      call(Test),
      format('Test ~w passed.~n', [Test]).
  
  run_test(Test) :-
      \+ call(Test),
      format('Test ~w failed.~n', [Test]).
  

%test cases:

%expr
% Test Case 1: Check if var('X') is a valid expression
test_expr1 :- expr(var('X')).
% Test Case 2: Check if var('X') is a valid expression
test_expr2 :- expr(lam('Parameter', bool, and(var('Parameter'), true_))).
% Test Case 3: Check if app(X, Y) is a valid application expression
test_expr3 :- expr(app(lam('Parameter', bool, var('Parameter')), true_)).
% Test Case 4: Check if pair(X, Y) is a valid pair expression
test_expr4 :- expr(pair(true_, lam('Parameter', bool, var('Parameter')))).
% Test Case 5: Check if fst(X) is a valid fst operation
test_expr5 :- expr(fst(pair(true_, lam('Parameter', bool, var('Parameter'))))).
% Test Case 6: Check if snd(X) is a valid snd operation with a different expression
test_expr6 :- expr(snd(pair(lam('Param', bool, var('Param')), and(true_, false_)))).
% Test Case 7: Check if and(X, Y) is a valid snd operation with a different expression
test_expr7 :- expr(and(false_, true_)).
% Test Case 8: Check if if_then_else(B, T, E) is a valid if-then-else expression
test_expr8 :- expr(if_then_else(and(true_, false_), true_, false_)).
% Test Case 9: Check if let_in(V, E1, E2) is a valid let-in expression
test_expr9 :- expr(let('X', and(true_, false_), and(true_, false_))).

%value
% Test Case 1: Check if a lambda expression is a valid value
test_value1 :- value(lam('Parameter', bool, and(var('Parameter'), true_))).
% Test Case 2: Check if a boolean value is a valid value
test_value2 :- value(bool(true)).
% Test Case 3: Check if a pair of values is a valid value
test_value3 :- value(pair(bool(true), bool(false))).

%type
% Test Case 1: Check if bool is a valid type
test_type1 :- type(bool).
% Test Case 2: Check if nat is a valid type
test_type2 :- type(nat).
% Test Case 3: Check if func(bool, nat) is a valid type
test_type3 :- type(func(bool, nat)).
% Test Case 4: Check if pair(bool, func(nat, bool)) is a valid type
test_type4 :- type(pair(bool, func(nat, bool))).

%substitute test cases:
% Substitution Test Case 1: Substitute variable 'X' with 5
test_subst1 :- subst(x, num(5), x, num(5)).
% Substitution Test Case 2: No substitution when variable 'y' is different
test_subst2 :- subst(x, num(5), y, y).
% Substitution Test Case 3: Substitute variable 'x' with num(5) in application 'app(x, y)'
test_subst3 :- subst(x, num(5), app(x, y), app(num(5), y)).
% Substitution Test Case 4: Substitute variable 'x' with num(5) in application 'app(z, x)'
test_subst4 :- subst(x, num(5), app(z, x), app(z, num(5))).
% Substitution Test Case 5: No substitution in lambda abstraction 'lam(x, nat, x)'
test_subst5 :- subst(x, num(5), lam(x, nat, x), lam(x, nat, x)).

%typed
% Test Case 1: Check if 'true_' has type bool
test_typed1 :- typed([], true_, bool).
% Test Case 2: Check if 'false_' has type bool
test_typed2 :- typed([], false_, bool).
% Test Case 3: Check if a lambda expression has the correct arrow type in an empty context
test_typed3 :- typed([], lam('Parameter', bool, and(var('Parameter'), true_)), arrow(bool, bool)).
% Test Case 4: Check if a lambda expression has the correct arrow type in an empty context
test_typed4 :- \+ typed([], lam('Parameter', bool, and(var('Parameter'), true_)), false).
% Test Case 5: Check if an application has the correct type in a context
test_typed5 :- typed([('X', arrow(bool, bool)), ('Y', bool)], app(var('X'), var('Y')), bool).
% Test Case 6: Check if a pair expression has the correct pair type in an empty context
test_typed6 :- typed([], pair(true_, lam('Parameter', bool, var('Parameter'))), pair(bool, arrow(bool, bool))).
% Test Case 7: Check if the first part of a pair has the correct type in an empty context
test_typed7 :- typed([], fst(pair(true_, lam('Parameter', bool, var('Parameter')))), bool).
% Test Case 8: Check if the second part of a pair has the correct type in an empty context
test_typed8 :- typed([], snd(pair(true_, lam('Parameter', bool, var('Parameter')))), arrow(bool, bool)).
% Test Case 9: Check if an 'and' expression has the correct type in an empty context
test_typed9 :- typed([], and(false_, true_), bool).
% Test Case 10: Check if an 'if_then_else' expression has the correct type in an empty context
test_typed10 :- typed([], if_then_else(false_, true_, false_), bool).


% delete this it doesn't work
% Test Case 11: % Assuming 'Y' is a function from bool to bool
% Revised Test Case 11:
test_typed11 :- \+ typed([('X', bool), ('Y', arrow(bool, bool))], 
      let('Z', and(true_, false_), app(var('Y'), var('Z'))), 
      arrow(bool, bool)).

% Assume a context [('X', bool), ('Y', arrow(bool, bool))]
% Test Case 1: Check if 'X' is of type bool in the given context
test_typed_context1 :- typed([('X', bool), ('Y', arrow(bool, bool))], var('X'), bool).
% Test Case 2: Check if 'Y' applied to 'true_' has the correct type in the given context
test_typed_context2 :- typed([('X', bool), ('Y', arrow(bool, bool))], app(var('Y'), true_), bool).
% Test Case 3: Check if 'Y' applied to 'false_' has the correct type in the given context
test_typed_context3 :- typed([('X', bool), ('Y', arrow(bool, bool))], app(var('Y'), false_), bool).

%delete this, it doesn't work.
% Test Case 4: Check if 'let' expression in the given context has the correct type
test_typed_context4  :- typed([('X', bool), ('Y', arrow(bool, bool))], let('Z', var('Y'), app(var('Z'), false_)), arrow(bool, bool)).

%Sstep

% Test Case 1: Reducing logical AND when the first part is true
test_sstep1 :- sstep(and(true, false), false).
% Test Case 2: Reducing logical AND when the second part is true
test_sstep2 :- sstep(and(false, true), false).
% Test Case 3: Reducing logical AND when both parts are true
test_sstep3 :- sstep(and(true, true), true).
% Test Case 4: Reducing logical AND when both parts are false
test_sstep4 :- sstep(and(false, false), false).
% Test Case 5: Reducing if-then-else when the condition is true
test_sstep5 :- sstep(if_then_else(true, 42, 99), 42).
% Test Case 6: Reducing if-then-else when the condition is false
test_sstep6 :- sstep(if_then_else(false, 42, 99), 99).
% Test Case 7: Reducing let expression with a value as the bound expression
test_sstep7 :- sstep(app(lam(_A, _, app(_B, _C)), lam(_, _, _)), app(lam(_D, _, _), lam(_, _, _))).
% Test Case 8: Reducing let expression with a reducible bound expression
test_sstep8 :- sstep(let('X', app(lam('A', bool, var('A')), lam('B', 'C', 'D')), var('X')), var('X')).    
% Test Case 9: Reducing if-then-else with a reducible condition
test_sstep9 :- sstep(if_then_else(and(true, false), 42, 99), if_then_else(false, 42, 99)).
% Test Case 10: Reducing application where the function is a value and the argument is reducible
test_sstep10 :- sstep(app(lam('X', bool, and(var('X'), true)), and(false, true)), app(lam('X', bool, and(var('X'), true)), false)).
% Test Case 11: Reducing pair expression where both parts can be further reduced
test_sstep11 :-  sstep(pair(and(true, false), and(true, false)), pair(false, false)).
% Test Case 12: Reducing fst operation on a pair where the pair itself can be further reduced
test_sstep12 :- sstep(fst(pair(bool(true), false)), bool(true)).


sstep(snd(pair(true, true)), true).
%someone come and explain this

%mstep
%Test Case 1: Basic Lambda that cant be reduced
test_mstep1 :- mstep(lam(true, true, true), lam(true, true, true)).

%type deriv
% Test Case 1: Check if 'true' has type bool
test_typederiv1 :- typederiv(bool(true), bool, _).
% Test Case 2: Check if 'true' has type bool
test_typederiv2 :- typederiv(bool(true), bool, t_BoolTrue).
% Test Case 3: Check if 'false' has type bool
test_typederiv3 :- typederiv(bool(false), bool, t_BoolFalse).
% Test Case 4: Check if 'and(true, false)' has type bool
test_typederiv4 :- typederiv(and(bool(true), bool(false)), bool, _).
% Test Case 5: Check if 'and(true, false)' has type bool
test_typederiv5 :- typederiv(and(bool(true), bool(false)), bool, t_And(t_BoolTrue, t_BoolFalse)).
% Test Case 6: Check if 'and(true, false)' has type bool
test_typederiv6 :- \+ typederiv(and(bool(true), bool(false)), bool, t_BoolFalse).
% Test Case 7: Check if 'fst(true)' does not have type bool (a negative test)
test_typederiv7 :- \+ typederiv(fst(bool(true)), bool, _).
% Test Case 8: Check if pair works when given a pair of true statements
test_typederiv8 :- typederiv(pair(bool(true), bool(true)), pair(bool, bool), _).
% Test Case 9: Check if pair works when given a pair of true statements, but with the proper type
test_typederiv9 :- typederiv(pair(bool(true), bool(true)), pair(bool, bool), t_Pair(t_BoolTrue, t_BoolTrue)).



% Test Case 1: Check if 'true' has type bool (a typed value)
test_typederiv10 :- typedvalue(bool(true), bool).


% Add this at the end of your file
:-  initialization(consult('a4Millad.pl')), 
    initialization(run_all_tests).
