/* roads_kb.pl
   Jamaican Rural Road Path Finder - Knowledge Base
   University of Technology Jamaica - AI Project
   
   This file stores the road network and implements pathfinding algorithms:
   - BFS (Breadth-First Search)
   - Dijkstra's Algorithm
   - A* Algorithm with GPS-based heuristics
*/

:- dynamic road/6.
:- dynamic coords/3.

/* ==================================================================
   GPS COORDINATES FOR JAMAICAN LOCATIONS
   Format: coords(Location, Latitude, Longitude)
   ================================================================== */

% Kingston and surrounding areas
coords(kingston, 17.9714, -76.7931).
coords(spanish_town, 17.9912, -76.7569).
coords(portmore, 17.9500, -76.8800).
coords(old_harbour, 17.9419, -77.1089).

% Clarendon
coords(may_pen, 17.9645, -77.2450).
coords(lionel_town, 17.8167, -77.2167).
coords(chapelton, 18.1500, -77.2000).

% Manchester
coords(mandeville, 18.0419, -77.5019).
coords(christiana, 18.1667, -77.4167).
coords(porus, 18.0833, -77.3833).

% St. Catherine
coords(linstead, 18.1392, -77.0317).
coords(bog_walk, 18.1000, -77.0000).
coords(ewarton, 18.1833, -77.0833).
coords(riversdale, 18.1167, -77.1500).

% St. Ann
coords(ocho_rios, 18.4079, -77.1030).
coords(st_anns_bay, 18.4167, -77.4167).
coords(brown_town, 18.3667, -77.3500).
coords(alexandria, 18.4000, -77.2167).
coords(runaway_bay, 18.4667, -77.3167).

% St. Mary
coords(port_maria, 18.3667, -76.8833).
coords(annotto_bay, 18.2667, -76.7667).
coords(gayle, 18.3167, -76.8500).

% St. Elizabeth
coords(santa_cruz, 18.0667, -77.7167).
coords(black_river, 18.0250, -77.8500).

/* ==================================================================
   ROAD NETWORK DATA
   Format: road(Source, Destination, DistanceKm, Type, Status, TimeMins)
   ================================================================== */

% Kingston Metropolitan Area
road(kingston, spanish_town, 21, paved, open, 25).
road(kingston, portmore, 17, paved, open, 22).
road(spanish_town, portmore, 12, paved, open, 15).
road(portmore, old_harbour, 28, paved, open, 35).

% Spanish Town to Central
road(spanish_town, linstead, 23, paved, open, 30).
road(spanish_town, old_harbour, 19, paved, open, 25).
road(linstead, bog_walk, 9, paved, open, 12).
road(bog_walk, ewarton, 8, unpaved, open, 15).
road(ewarton, linstead, 11, paved, open, 14).

% Clarendon Region
road(old_harbour, may_pen, 23, paved, open, 28).
road(may_pen, lionel_town, 19, unpaved, open, 30).
road(may_pen, chapelton, 21, paved, open, 25).
road(lionel_town, chapelton, 18, deep_potholes, open, 35).

% Manchester Region
road(may_pen, mandeville, 37, paved, open, 45).
road(mandeville, christiana, 20, paved, open, 25).
road(mandeville, porus, 15, paved, open, 18).
road(porus, may_pen, 28, unpaved, open, 40).

% St. Catherine Interior
road(linstead, riversdale, 14, unpaved, seasonal, 25).
road(riversdale, chapelton, 16, broken_cisterns, open, 30).
road(ewarton, riversdale, 12, deep_potholes, open, 22).

% North Coast - St. Ann
road(ocho_rios, st_anns_bay, 15, paved, open, 18).
road(st_anns_bay, brown_town, 15, paved, open, 20).
road(brown_town, alexandria, 18, unpaved, open, 30).
road(alexandria, runaway_bay, 22, paved, open, 25).
road(runaway_bay, ocho_rios, 18, paved, open, 20).

% St. Ann to St. Mary
road(ocho_rios, port_maria, 35, paved, open, 40).
road(port_maria, annotto_bay, 25, paved, open, 30).
road(annotto_bay, kingston, 58, paved, open, 75).

% Mountain/Rural Roads (more challenging)
road(brown_town, christiana, 28, unpaved, seasonal, 50).
road(alexandria, linstead, 32, deep_potholes, open, 55).
road(bog_walk, gayle, 45, broken_cisterns, closed, 70).
road(ewarton, brown_town, 35, unpaved, under_repair, 60).

% Emergency alternate routes (now using defined coords)
road(mandeville, santa_cruz, 25, paved, open, 30).
road(santa_cruz, black_river, 35, paved, open, 42).

/* ==================================================================
   BIDIRECTIONAL EDGE REPRESENTATION
   Makes all roads accessible in both directions
   ================================================================== */

edge(U, V, Dist, Type, Status, Time) :-
    road(U, V, Dist, Type, Status, Time).
    
edge(U, V, Dist, Type, Status, Time) :-
    road(V, U, Dist, Type, Status, Time).

/* ==================================================================
   CRITERIA FILTERING
   Determines if an edge is allowed based on user criteria
   ================================================================== */

edge_allowed(Type, Status, Criteria) :-
    % Check if we should avoid unpaved roads
    \+ (member(avoid(unpaved), Criteria), Type = unpaved),
    
    % Check if we should avoid closed roads
    \+ (member(avoid(closed), Criteria), Status = closed),
    
    % Check if we should avoid roads under repair
    \+ (member(avoid(under_repair), Criteria), Status = under_repair),
    
    % Check if we should avoid seasonal roads
    \+ (member(avoid(seasonal), Criteria), Status = seasonal),
    
    % Check if we should avoid deep potholes
    \+ (member(avoid(deep_potholes), Criteria), Type = deep_potholes),
    
    % Check if we should avoid broken cisterns
    \+ (member(avoid(broken_cisterns), Criteria), Type = broken_cisterns),
    
    % Preference for highways (if specified)
    \+ (member(prefer(paved), Criteria), Type \= paved),
    
    true.

% Helper predicate to get allowed edges
allowed_edge(U, V, Dist, Type, Status, Time, Criteria) :-
    edge(U, V, Dist, Type, Status, Time),
    edge_allowed(Type, Status, Criteria).

/* ==================================================================
   HEURISTIC CALCULATION - GPS-Based Distance
   Uses Haversine formula for accurate distance estimation
   ================================================================== */

% Convert degrees to radians
deg_to_rad(Deg, Rad) :- 
    Rad is Deg * pi / 180.

% Haversine formula - calculates great-circle distance between two points
haversine_distance(Lat1, Lon1, Lat2, Lon2, DistKm) :-
    deg_to_rad(Lat1, Lat1R),
    deg_to_rad(Lat2, Lat2R),
    deg_to_rad(Lon1, Lon1R),
    deg_to_rad(Lon2, Lon2R),
    
    DLat is Lat2R - Lat1R,
    DLon is Lon2R - Lon1R,
    
    A is sin(DLat/2) ** 2 + cos(Lat1R) * cos(Lat2R) * sin(DLon/2) ** 2,
    C is 2 * atan2(sqrt(A), sqrt(1-A)),
    
    EarthRadiusKm = 6371,
    DistKm is EarthRadiusKm * C.

% Calculate heuristic between two nodes
heuristic(Node, Goal, H) :-
    Node \= Goal,
    coords(Node, Lat1, Lon1),
    coords(Goal, Lat2, Lon2),
    !,
    haversine_distance(Lat1, Lon1, Lat2, Lon2, H).

% Same node = zero distance
heuristic(Node, Node, 0) :- !.

% Fallback if coordinates not found
heuristic(_, _, 50).

/* ==================================================================
   ALGORITHM 1: BREADTH-FIRST SEARCH (BFS)
   Finds path with fewest hops (unweighted search)
   ================================================================== */

bfs(Start, Goal, Criteria, Path, Distance, Time) :-
    bfs_queue([[Start]], Goal, Criteria, RevPath),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, Distance, Time).

% BFS queue processing
bfs_queue([[Goal|RestPath]|_], Goal, _Criteria, [Goal|RestPath]) :- !.

bfs_queue([CurrentPath|OtherPaths], Goal, Criteria, SolutionPath) :-
    CurrentPath = [CurrentNode|_],
    findall([Next, CurrentNode|CurrentPath],
            (allowed_edge(CurrentNode, Next, _, _, _, _, Criteria),
             \+ member(Next, CurrentPath)),
            NewPaths),
    append(OtherPaths, NewPaths, UpdatedQueue),
    bfs_queue(UpdatedQueue, Goal, Criteria, SolutionPath).

/* ==================================================================
   UTILITY: Calculate total distance and time for a path
   ================================================================== */

path_distance_time([_], _Criteria, 0, 0).

path_distance_time([A,B|Rest], Criteria, TotalDist, TotalTime) :-
    allowed_edge(A, B, Dist, _, _, Time, Criteria),
    path_distance_time([B|Rest], Criteria, SubDist, SubTime),
    TotalDist is Dist + SubDist,
    TotalTime is Time + SubTime.

/* ==================================================================
   ALGORITHM 2: DIJKSTRA'S ALGORITHM
   Finds shortest distance path (weighted search)
   ================================================================== */

dijkstra(Start, Goal, Criteria, Path, Distance, Time) :-
    dijkstra_search([node(Start, [Start], 0)], [], Goal, Criteria, 
                    node(Goal, RevPath, Distance)),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, _CheckDist, Time).

% Dijkstra search - goal found
dijkstra_search(Open, _Closed, Goal, _Criteria, node(Goal, Path, Dist)) :-
    maplist(add_key_g, Open, Keyed),
    keysort(Keyed, [_-node(Goal, Path, Dist)|_]),
    !.

% Dijkstra search - continue searching
dijkstra_search(Open, Closed, Goal, Criteria, Result) :-
    maplist(add_key_g, Open, Keyed),
    keysort(Keyed, [_-node(Node, Path, G)|RestKeyed]),
    strip_keys(RestKeyed, RestOpen),
    
    findall(node(Next, [Next|Path], G1),
            (allowed_edge(Node, Next, StepDist, _, _, _, Criteria),
             \+ member(Next, Path),
             G1 is G + StepDist),
            Children),
    
    append(RestOpen, Children, NewOpen),
    dijkstra_search(NewOpen, [Node|Closed], Goal, Criteria, Result).

% Helper predicates for Dijkstra
add_key_g(node(Node, Path, G), G-node(Node, Path, G)).

strip_keys([], []).
strip_keys([_-Node|Rest], [Node|RestNodes]) :-
    strip_keys(Rest, RestNodes).

/* ==================================================================
   ALGORITHM 3: FASTEST TIME SEARCH
   Uses travel time as the weight instead of distance
   ================================================================== */

fastest_time(Start, Goal, Criteria, Path, Distance, Time) :-
    fastest_search([node(Start, [Start], 0)], [], Goal, Criteria,
                   node(Goal, RevPath, Time)),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, Distance, _CheckTime).

% Fastest time search - goal found
fastest_search(Open, _Closed, Goal, _Criteria, node(Goal, Path, Time)) :-
    maplist(add_key_g, Open, Keyed),
    keysort(Keyed, [_-node(Goal, Path, Time)|_]),
    !.

% Fastest time search - continue searching
fastest_search(Open, Closed, Goal, Criteria, Result) :-
    maplist(add_key_g, Open, Keyed),
    keysort(Keyed, [_-node(Node, Path, T)|RestKeyed]),
    strip_keys(RestKeyed, RestOpen),
    
    findall(node(Next, [Next|Path], T1),
            (allowed_edge(Node, Next, _, _, _, StepTime, Criteria),
             \+ member(Next, Path),
             T1 is T + StepTime),
            Children),
    
    append(RestOpen, Children, NewOpen),
    fastest_search(NewOpen, [Node|Closed], Goal, Criteria, Result).

/* ==================================================================
   ALGORITHM 4: A* SEARCH
   Heuristic-guided search for optimal pathfinding
   ================================================================== */

astar(Start, Goal, Criteria, Path, Distance, Time) :-
    heuristic(Start, Goal, H0),
    astar_search([node(Start, [Start], 0, H0)], Goal, Criteria,
                 node(Goal, RevPath, Distance, _)),
    reverse(RevPath, Path),
    path_distance_time(Path, Criteria, _CheckDist, Time).

% A* search - goal found
astar_search(Open, Goal, _Criteria, node(Goal, Path, G, F)) :-
    maplist(add_key_f, Open, Keyed),
    keysort(Keyed, [_-node(Goal, Path, G, F)|_]),
    !.

% A* search - continue searching
astar_search(Open, Goal, Criteria, Result) :-
    maplist(add_key_f, Open, Keyed),
    keysort(Keyed, [_-node(Node, Path, G, _F)|RestKeyed]),
    strip_keys(RestKeyed, RestOpen),
    
    findall(node(Next, [Next|Path], G1, F1),
            (allowed_edge(Node, Next, StepDist, _, _, _, Criteria),
             \+ member(Next, Path),
             G1 is G + StepDist,
             heuristic(Next, Goal, H1),
             F1 is G1 + H1),
            Children),
    
    append(RestOpen, Children, NewOpen),
    astar_search(NewOpen, Goal, Criteria, Result).

% Helper for A*
add_key_f(node(Node, Path, G, F), F-node(Node, Path, G, F)).

/* ==================================================================
   HIGH-LEVEL API FOR PYTHON INTERFACE
   Main entry point for pathfinding requests
   ================================================================== */

find_route(shortest_distance, Start, Goal, Criteria, Path, Distance, Time) :-
    dijkstra(Start, Goal, Criteria, Path, Distance, Time).

find_route(fastest_time, Start, Goal, Criteria, Path, Distance, Time) :-
    fastest_time(Start, Goal, Criteria, Path, Distance, Time).

find_route(bfs, Start, Goal, Criteria, Path, Distance, Time) :-
    bfs(Start, Goal, Criteria, Path, Distance, Time).

find_route(astar, Start, Goal, Criteria, Path, Distance, Time) :-
    astar(Start, Goal, Criteria, Path, Distance, Time).

/* ==================================================================
   NETWORK STATISTICS (NEW SECTION ADDED TO FIX ERROR)
   ================================================================== */

% Count total number of unique locations (by counting 'coords' facts)
count_locations(Count) :-
    findall(L, coords(L, _, _), Locations),
    length(Locations, Count).

% Count total number of roads (by counting 'road' facts)
count_roads(Count) :-
    findall(R, road(_, _, _, _, _, _), Roads),
    length(Roads, Count).

% Collect all network statistics for Python integration
get_network_stats(LocCount, RoadCount) :-
    count_locations(LocCount),
    count_roads(RoadCount).

/* ==================================================================
   ADMIN FUNCTIONS - Dynamic Network Updates
   ================================================================== */

% Add a new road to the network
add_road(Source, Dest, DistanceKm, Type, Status, TimeMins) :-
    assertz(road(Source, Dest, DistanceKm, Type, Status, TimeMins)),
    format('Road added: ~w -> ~w (~w km, ~w, ~w)~n', 
           [Source, Dest, DistanceKm, Type, Status]).

% Update road status
update_road_status(Source, Dest, NewStatus) :-
    retract(road(Source, Dest, Dist, Type, _OldStatus, Time)),
    assertz(road(Source, Dest, Dist, Type, NewStatus, Time)),
    format('Road status updated: ~w -> ~w is now ~w~n', 
           [Source, Dest, NewStatus]).

% Add GPS coordinates for a location
add_location(Location, Latitude, Longitude) :-
    assertz(coords(Location, Latitude, Longitude)),
    format('Location added: ~w at (~w, ~w)~n', 
           [Location, Latitude, Longitude]).

% List all roads from a location
list_roads_from(Location) :-
    format('Roads from ~w:~n', [Location]),
    forall(road(Location, Dest, Dist, Type, Status, Time),
           format('  -> ~w: ~w km, ~w, ~w, ~w mins~n', 
                  [Dest, Dist, Type, Status, Time])).

% Get all unique locations in the network
get_all_locations(Locations) :-
    findall(L, coords(L, _, _), Locations).

/* ==================================================================
   END OF KNOWLEDGE BASE
   ================================================================== */