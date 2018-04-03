% candidate_number(12345).

%% % Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)

find_identity(A):-
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



