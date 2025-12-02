# Rural Road Path Finder – Python + Prolog

This mini–project shows how to integrate **Python** and **Prolog** to find
paths in a rural road network using different AI search strategies:
- **BFS** (fewest hops)
- **Dijkstra** (shortest distance)
- **A*** (heuristic search)

## Project structure

```text
RuralRouteFinderProject/
  prolog/
    roads_kb.pl       # Prolog knowledge base and search algorithms
  python/
    main.py           # Python text–menu interface
```

## Requirements

- Python 3.x
- SWI–Prolog installed
- `swipl` available on your system PATH

If `swipl` is not on PATH, open `python/main.py` and change:

```python
SWIPL_CMD = "swipl"
```

to the full path of your SWI–Prolog executable, e.g.:

```python
SWIPL_CMD = r"C:\\Program Files\\swipl\\bin\\swipl.exe"
```

## How to run

1. Open a terminal in the **RuralRouteFinderProject** folder.
2. Run:

   ```bash
   cd python
   python main.py
   ```

3. Use the menu:
   - Choose **start** and **goal** nodes (e.g. `a` and `d`).
   - Select a search algorithm:
     - `1` – shortest distance (Dijkstra)
     - `2` – fastest time (reuses Dijkstra weighting)
     - `3` – BFS
     - `4` – A*
   - Choose optional criteria:
     - `avoid unpaved`
     - `avoid closed`

The program then calls Prolog, which:
- filters roads according to your criteria,
- runs the requested algorithm,
- returns a path, total distance, and travel time.

## Customising the map

Edit `prolog/roads_kb.pl`:

- Replace the example `road/6` facts with your **real villages** and
  **distances**.
- Extend `edge_allowed/3` with more conditions
  (e.g. `deep_potholes`, `landslide`, `flood_prone`).
- Adjust the `heuristic/3` clauses to give realistic distance estimates
  between nodes for A*.

Example of a road fact:

```prolog
road(portmore, spanish_town, 12, paved, open, 20).
```

Remember to keep the node names as **lowercase atoms without spaces**
(use underscores if needed, e.g. `golden_spring`).

## Notes

- The admin option “Add road” in the Python menu adds a road only in the
  **current Prolog runtime**. To make it permanent, add the same `road/6`
  fact directly into `prolog/roads_kb.pl`.
- This structure aligns well with assignments that require:
  - modelling the road network with facts,
  - using search strategies (BFS, Dijkstra, A*),
  - implementing criteria/constraints,
  - and integrating a Python interface with Prolog reasoning.
