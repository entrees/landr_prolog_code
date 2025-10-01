
/*
 * import grammar
 */

:- ['grammar.pl'].

/*
 * pp_ptree routine
 */

pp_ptree_list([],_,_).
pp_ptree_list([node(Label1,[node(Label2,[])])|T],Indent,Embedding) :-
  !,
  ( Embedding == 0 -> write(' (') ; nl, tab(Indent), write('(') ),
  write(Label1),
  write(' '),
  write(Label2),
  write(')'),
  pp_ptree_list(T,Indent,Embedding).
pp_ptree_list([node(Label,NodeList)|T],Indent,_) :-
  pp_nonpreterminal_node(Label,NodeList,Indent),
  ( Indent == 0 -> nl ; true ),
  pp_ptree_list(T,Indent,1).

pp_nonpreterminal_node(Label,NodeList,Indent) :-
  nl,
  tab(Indent),
  write('('),
  write(Label),
  NextIndent is Indent+2,
  pp_ptree_list(NodeList,NextIndent,0),
  write(')').

/*
 * main parse routine
 */

:- import append/3 from basics.

parse(Goal) :-
  Goal =.. L1,
  append(L1,[[X],[]],L2),
  Goal2 =.. L2,
  setof(X, tphrase(Goal2), R),
  pp_ptree_list(R,0,0).
parse(Goal) :-
  Goal =.. L1,
  append(L1,[[X,Y|T],[]],L2),
  Goal2 =.. L2,
  setof(node('',[X,Y|T]), tphrase(Goal2), R),
  pp_ptree_list(R,0,0).

