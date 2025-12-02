/* roads_kb.pl
   Rural Road Path Finder using BFS, Dijkstra, and A* in Prolog
   You can adapt this file with real Jamaican villages and road attributes.
*/

:- dynamic road/6.
% road(Source, Dest, DistanceKm, Type, Status, TimeMins).

/* ------------------------------------------------------------------
   Example road network
   Replace these with your actual villages/towns and distances.
   Nodes are simple lowercase atoms: a, b, c, d, e ...
   ------------------------------------------------------------------ */

road(a, b, 5,  paved,   open,   7).
road(b, c, 3,  unpaved, open,   6).
road(c, d, 7,  paved,   closed, 10).
road(b, d, 4,  paved,   open,   5).
road(a, e, 2,  unpaved, open,   4).
road(e, d, 9,  paved,   open,   12).

/* If roads are two-way, expose them as undirected edges. */
edge(U, V, Dist, Type, Status, Time) :-
    road(U, V, Dist, Type, Status, Time).
edge(U, V, Dist, Type, Status, Time) :-
    road(V, U, Dist, Type, Status, Time).

/* ------------------------------------------------------------------
   Criteria filtering
   Criteria is a list like: [avoid(unpaved), avoid(closed)]
   Extend this with extra tags (deep_potholes, landslide, etc.)
   ------------------------------------------------------------------ */

edge_allowed(Type, Status, Criteria) :-
    \+ ( member(avoid(unpaved), Criteria), Type = unpaved ),
    \+ ( member(avoid(closed),  Criteria), Status = closed ),
    true.

allowed_edge(U, V, Dist, Type, Status, Time, Criteria) :-
    edge(U, V, Dist, Type, Status, Time),
    edge_allowed(Type, Status, Criteria).

/* ------------------------------------------------------------------
   1. BFS – simple unweighted search (fewest hops)
   ------------------------------------------------------------------ */

bfs(Start, Goal, Criteria, Path, Distance, Time) :-
    bfs_queue([[Start]], Goal, Criteria, RevPath),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, Distance, Time).

bfs_queue([[Goal|RestPath]|_], Goal, _Criteria, [Goal|RestPath]) :- !.
bfs_queue([CurrentPath|OtherPaths], Goal, Criteria, SolutionPath) :-
    CurrentPath = [CurrentNode|_],
    findall([Next,CurrentNode|CurrentPath],
            ( allowed_edge(CurrentNode, Next, _, _, _, _, Criteria),
              \+ member(Next, CurrentPath)
            ),
            NewPaths),
    append(OtherPaths, NewPaths, UpdatedQueue),
    bfs_queue(UpdatedQueue, Goal, Criteria, SolutionPath).

/* ------------------------------------------------------------------
   Utility – compute total distance and time for a path
   ------------------------------------------------------------------ */

path_distance_time([_], _Criteria, 0, 0).
path_distance_time([A,B|Rest], Criteria, TotalDist, TotalTime) :-
    allowed_edge(A, B, Dist, _, _, Time, Criteria),
    path_distance_time([B|Rest], Criteria, SubDist, SubTime),
    TotalDist is Dist + SubDist,
    TotalTime is Time + SubTime.

/* ------------------------------------------------------------------
   2. Dijkstra – shortest distance (by DistanceKm)
   Open list: node(Node, PathSoFar, G)
   ------------------------------------------------------------------ */

dijkstra(Start, Goal, Criteria, Path, Distance, Time) :-
    dijkstra_search([node(Start, [Start], 0)], [], Goal, Criteria, node(Goal, RevPath, Distance)),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, _CheckDist, Time).

dijkstra_search(Open, _Closed, Goal, _Criteria, node(Goal, Path, Dist)) :-
    % pick best from open
    maplist(add_key_g, Open, Keyed),
    keysort(Keyed, [_-node(Goal, Path, Dist)|_]),
    !.
dijkstra_search(Open, Closed, Goal, Criteria, Result) :-
    maplist(add_key_g, Open, Keyed),
    keysort(Keyed, [_-node(Node, Path, G)|RestKeyed]),
    strip_keys(RestKeyed, RestOpen),
    findall(node(Next, [Next|Path], G1),
            ( allowed_edge(Node, Next, StepDist, _, _, _, Criteria),
              \+ member(Next, Path),
              G1 is G + StepDist
            ),
            Children),
    append(RestOpen, Children, NewOpen),
    dijkstra_search(NewOpen, [Node|Closed], Goal, Criteria, Result).

add_key_g(node(Node, Path, G), G-node(Node, Path, G)).
strip_keys([], []).
strip_keys([_-Node|Rest], [Node|RestNodes]) :-
    strip_keys(Rest, RestNodes).

/* ------------------------------------------------------------------
   3. A* – heuristic-guided search
   Open list: node(Node, PathSoFar, G, F)
   F = G + H (G is distance so far, H is heuristic)
   ------------------------------------------------------------------ */

% Heuristic estimates: YOU should adjust these for your real map.
% heuristic(Node, Goal, EstimatedDistanceKm).

heuristic(a, d, 6).
heuristic(b, d, 4).
heuristic(c, d, 2).
heuristic(e, d, 5).
heuristic(Node, Node, 0) :- !.
heuristic(_, _, 5).  % default fallback estimate

astar(Start, Goal, Criteria, Path, Distance, Time) :-
    heuristic(Start, Goal, H0),
    astar_search([node(Start, [Start], 0, H0)], Goal, Criteria, node(Goal, RevPath, Distance, _)),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, _CheckDist, Time).

astar_search(Open, Goal, _Criteria, node(Goal, Path, G, F)) :-
    maplist(add_key_f, Open, Keyed),
    keysort(Keyed, [_-node(Goal, Path, G, F)|_]),
    !.
astar_search(Open, Goal, Criteria, Result) :-
    maplist(add_key_f, Open, Keyed),
    keysort(Keyed, [_-node(Node, Path, G, _F)|RestKeyed]),
    strip_keys(RestKeyed, RestOpen),
    findall(node(Next, [Next|Path], G1, F1),
            ( allowed_edge(Node, Next, StepDist, _, _, _, Criteria),
              \+ member(Next, Path),
              G1 is G + StepDist,
              heuristic(Next, Goal, H1),
              F1 is G1 + H1
            ),
            Children),
    append(RestOpen, Children, NewOpen),
    astar_search(NewOpen, Goal, Criteria, Result).

add_key_f(node(Node, Path, G, F), F-node(Node, Path, G, F)).

/* ------------------------------------------------------------------
   High-level API for Python
   search type: shortest_distance | fastest_time | bfs | astar
   Criteria is a list like [avoid(unpaved),avoid(closed)]
   ------------------------------------------------------------------ */

find_route(shortest_distance, Start, Goal, Criteria, Path, Distance, Time) :-
    dijkstra(Start, Goal, Criteria, Path, Distance, Time).

% For simplicity, fastest_time reuses Dijkstra but you could make
% a variant that uses Time instead of Distance as the weight.
find_route(fastest_time, Start, Goal, Criteria, Path, Distance, Time) :-
    dijkstra(Start, Goal, Criteria, Path, Distance, Time).

find_route(bfs, Start, Goal, Criteria, Path, Distance, Time) :-
    bfs(Start, Goal, Criteria, Path, Distance, Time).

find_route(astar, Start, Goal, Criteria, Path, Distance, Time) :-
    astar(Start, Goal, Criteria, Path, Distance, Time).

/* ------------------------------------------------------------------
   Admin helper – add a road at runtime
   Note: to persist permanently, also edit this file manually.
   ------------------------------------------------------------------ */

add_road(Source, Dest, DistanceKm, Type, Status, TimeMins) :-
    assertz(road(Source, Dest, DistanceKm, Type, Status, TimeMins)).
