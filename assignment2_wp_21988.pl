candidate_number(21988).

%% % Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
%% part 2 implemented below

find_identity(A):-
  (part_module(2) -> find_identity_2(A)
  ; otherwise     -> find_identity_o(A)
  ).

find_identity_2(A):-
  findall(PotentialActor, actor(PotentialActor), PotentialActors),
  find_identity_1(A, PotentialActors),!.

find_identity_1(Actor, [Actor]). % Return when we have one answer
find_identity_1(Actor, PotentialActors) :-
  agent_ask_oracle(oscar, o(1), link, Link),
  find_actors_link(Link, PotentialActors, [], ReducedPotentials),
  find_identity_1(Actor, ReducedPotentials).

find_actors_link(_, [], ActorsLink, ActorsLink). % Once list exhausted
find_actors_link(Link, [Actor|Actors], UpdatedLink, ActorsLink) :-
  wp(Actor, WT),
  findall(X, wt_link(WT, X), Links),
  ( memberchk(Link, Links) -> find_actors_link(Link, Actors, [Actor|UpdatedLink], ActorsLink)
  ; otherwise              -> find_actors_link(Link, Actors, UpdatedLink, ActorsLink)
  ).


%% part3 below
find_identity_o(ActorIdentity) :- 
  findall(Actor, actor(Actor), Actors),
  Oracles = [1,2,3,4,5,6,7,8,9,10],
  find_actors(Actors, ActorIdentity, Oracles),!.

find_actors(Actors, ActorIdentity, Oracles):-
  Actors = [Actor],
  ActorIdentity = Actor.

find_actors(Actors, ActorIdentity, [Oracle | OraclesR] ) :-
  oracle_visitor(Oracle,Link),
  find_actors_link(Link,Actors,[], ReducedActors),
  find_actors(ReducedActors, ActorIdentity, OraclesR).

find_actors(Actors, ActorIdentity, []) :-
  ActorIdentity = Actors.

oracle_visitor(Oracle,Link) :-
  write("Computing path to: "),
  write(Oracle), nl,
  my_agent(Agent),
  get_energy(Energy),
  write("Energy: "), write(Energy), nl,
  Energy < 60,
  find_charger(PathToCharger),
  moveme(PathToCharger),
  topup(Agent,_),
  oracle_visitor(Oracle,Link).

oracle_visitor(Oracle,Link):-
  user:solve_task_3(find(o(Oracle)),Path),
  moveme(Path),
  writeln("asking question: "),
  ask_oracle(Oracle,Link).

get_energy(Energy):-
  my_agent(Agent),
  query_world(agent_current_energy,[Agent,Energy]).

find_charger(PathToCharger):-
  user:solve_task_3(find(c(_)),PathToCharger).

moveme(Path) :-
  my_agent(Agent),
  query_world(agent_do_moves, [Agent,Path] ).

topup(Agent,ID) :-
  query_world(agent_topup_energy,[Agent,c(ID)]).

ask_oracle(Oracle,Link):-
  my_agent(Agent),
  query_world(agent_ask_oracle,[Agent, o(Oracle), link, Link] ).

agent_position( Position ) :-
  my_agent( Agent ),
  query_world( agent_current_position, [ Agent, Position ] ).

tester(Num,Path) :-
  Num = 1,
  oracle_visitor(Num,Path),
  oracle_visitor(Num+1,Path).

visitall(2,P) :- oracle_visitor(2,P).
visitall(N,P) :- oracle_visitor(N,P),visitall(N+1,P).
