candidate_number(21988).

solve_task(Task,Cost) :-
  part_module(1) -> solve_task_1(Task, Cost).


solve_task_1(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,Position] ),
  calc_f_value(Task, Position, 0, F),
  solve_task_a_star(Task, [[c(FCost, 0, Position), Position]], ReversPath, Cost, _),!, % prune choice point for efficiency
  reverse(ReversPath,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%% A* search 
solve_task_a_star(Task,[Current|_],ReversPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,ReversPath,Cost,NewPos),
  length(ReversPath,Depth).
solve_task_a_star(Task,Agenda,ReversPath,Cost,NewPos) :-
  Agenda = [Current|AgendaTail], % Get current node
  find_children(Task, Current, Children),
  insert_many_in_agenda(Children, AgendaTail, NewAgenda),
  solve_task_a_star(Task, NewAgenda, ReversPath, Cost, NewPos).

insert_many_in_agenda([], Agenda, Agenda).
insert_many_in_agenda([Child|Children], Agenda, NewAgenda) :-
  insert_agenda(Child, Agenda, TempAgenda), % TempAgenda is Agenda without Child
  insert_many_in_agenda(Children, TempAgenda, NewAgenda).

insert_agenda(Node,Agenda,Agenda) :- repeat_node(Node,Agenda), ! .
insert_agenda(Node,[A|R],[Node,A|R]) :- cheaper_cost(Node,A), ! .
insert_agenda(Node,[A|R],[A|S]) :- insert_agenda(Node,R,S), !.
insert_agenda(Node,[],[Node]).


repeat_node([c(_, _, Position)|_], [[c(_, _, Position)|_]|_]).
cheaper_cost([c(FCost1, _, _)|_], [c(FCost2, _, _)|_]) :- FCost1 <  FCost2.

find_children(Task, Node, Children) :-
  Node = [c(_, G, NodePos)|Path],
  (bagof([c(ChildF, ChildG, ChildPos), ChildPos|Path],
    ( search(NodePos, ChildPos, ChildPos, C),
      \+ memberchk(ChildPos, Path), % Don't unclude if already visited
      ChildG is G + C,
      calc_f_value(Task, ChildPos, ChildG, ChildF)
    ), Children); Children = []). % Fill children or empty

calc_f_value(find(_), _, G, F) :-
  F is G.
calc_f_value(go(TargetPos), Position, G, F) :-
  map_distance(Position, TargetPos, H),
  F is G + H.


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task, Current,  Depth,RPath,  [cost(Cost),  depth(Depth)],  NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(_,F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(_,F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(_,Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(_,Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
