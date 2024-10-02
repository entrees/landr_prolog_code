
/*
 * import grammar
 */

:- ['grammar.pl'].

/*
 * pp_ptree routine
 */

pp_ptree_list([]).
pp_ptree_list([node(Label,[])|T]) :-
  write(Label),
  nl,
  pp_ptree_list(T).
pp_ptree_list([node(Label,[First|Rest])|T]) :-
  pp_nonterminal_node(Label,First,Rest,0),
  nl,
  pp_ptree_list(T).

pp_ptree_first_child(node(Label,[]), _) :-
  tab(1),
  write(Label).
pp_ptree_first_child(node(Label,[First|Rest]), Column) :-
  tab(1),
  pp_nonterminal_node(Label,First,Rest,Column).

pp_ptree_rest([], _) :-
  write(')').
pp_ptree_rest([node(Label,[First|Rest])|T], Column) :-
  nl,
  tab(Column),
  pp_nonterminal_node(Label,First,Rest,Column),
  pp_ptree_rest(T, Column).

pp_nonterminal_node(Label,First,Rest,Column) :-
  write('('),
  write(Label),
  atom_length(Label, More),
  NextColumn is Column+More+2,
  pp_ptree_first_child(First, NextColumn),
  pp_ptree_rest(Rest, NextColumn).

/*
 * main parse routine
 */

:- import append/3 from basics.

parse(Goal) :-
  Goal =.. L1,
  append(L1,[[X],[]],L2),
  Goal2 =.. L2,
  setof(X, tphrase(Goal2), R),
  pp_ptree_list(R).
parse(Goal) :-
  Goal =.. L1,
  append(L1,[[X,Y|T],[]],L2),
  Goal2 =.. L2,
  setof(node('',[X,Y|T]), tphrase(Goal2), R),
  pp_ptree_list(R).

