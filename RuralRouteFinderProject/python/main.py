import subprocess

PROLOG_FILE = "prolog/roads_kb.pl"
SWIPL_CMD = "swipl"  # Update this to full path if swipl is not in PATH

def call_prolog_find_route(algo, start, goal, criteria):
    """Calls Prolog:
      find_route(Algo, Start, Goal, CriteriaList, Path, Distance, Time).
    Returns (path_str, distance, time) or (None, None, None) if no route.
    """
    criteria_terms = ",".join(criteria)
    criteria_list = f"[{criteria_terms}]" if criteria_terms else "[]"

    # We assume start/goal are atoms like a, b, c (no quotes, no spaces).
    goal_term = (
        f"find_route({algo},{start},{goal},{criteria_list},Path,Distance,Time),"
        "write(Path),write(' '),write(Distance),write(' '),write(Time),nl"
    )

    cmd = [SWIPL_CMD, "-q", "-s", PROLOG_FILE, "-g", goal_term, "-t", "halt"]

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=20)
    except FileNotFoundError:
        print("Error: SWI-Prolog (swipl) not found. Update SWIPL_CMD in main.py.")
        return None, None, None

    if result.returncode != 0 or not result.stdout.strip():
        print("No route found or Prolog failed.")
        if result.stderr:
            print("[Prolog error]", result.stderr.strip())
        return None, None, None

    line = result.stdout.strip().splitlines()[0].strip()
    # Expecting: [a,b,d] 9 12
    try:
        parts = line.split()
        path_str = parts[0]
        distance = float(parts[1])
        time_minutes = float(parts[2])
        return path_str, distance, time_minutes
    except Exception:
        print("Failed to parse Prolog output:", line)
        return None, None, None


def choose_algo():
    print("\nChoose search algorithm:")
    print("1. Shortest distance (Dijkstra)")
    print("2. Fastest time (reuse Dijkstra)")
    print("3. BFS (fewest hops)")
    print("4. A* (heuristic)")
    choice = input("Enter 1, 2, 3, or 4: ").strip()
    if choice == "1":
        return "shortest_distance"
    elif choice == "2":
        return "fastest_time"
    elif choice == "3":
        return "bfs"
    else:
        return "astar"


def choose_criteria():
    criteria = []
    print("\nCriteria (type y to apply, anything else to skip):")
    if input("Avoid unpaved roads? (y/n): ").lower().startswith("y"):
        criteria.append("avoid(unpaved)")
    if input("Avoid closed roads? (y/n): ").lower().startswith("y"):
        criteria.append("avoid(closed)")
    # Extend with more, e.g.:
    # if input("Avoid deep potholes? (y/n): ").lower().startswith("y"):
    #     criteria.append("avoid(deep_potholes)")
    return criteria


def main_menu():
    while True:
        print("\n=== Rural Road Path Finder ===")
        print("1. Find route")
        print("2. Add road (admin)")
        print("3. Exit")
        choice = input("Select option: ").strip()

        if choice == "1":
            start = input("Enter start node (e.g. a): ").strip().lower()
            goal = input("Enter goal node (e.g. d): ").strip().lower()

            algo_functor = choose_algo()
            criteria = choose_criteria()

            path_str, distance, time_minutes = call_prolog_find_route(
                algo_functor, start, goal, criteria
            )

            if path_str is None:
                print("\nNo path found or an error occurred.")
            else:
                print("\n=== Route Found ===")
                print("Raw Prolog path term:", path_str)
                print("Total distance (km):", distance)
                print("Estimated time (mins):", time_minutes)

        elif choice == "2":
            add_road_admin()
        elif choice == "3":
            print("Goodbye.")
            break
        else:
            print("Invalid option, try again.")


def add_road_admin():
    print("\n=== Add Road (Admin) ===")
    s = input("Source node (e.g. a): ").strip().lower()
    d = input("Destination node (e.g. b): ").strip().lower()
    dist = input("Distance in km (number): ").strip()
    rtype = input("Road type (paved/unpaved/...): ").strip().lower()
    status = input("Status (open/closed): ").strip().lower()
    time_mins = input("Travel time in minutes (number): ").strip()

    goal_term = (
        f"add_road({s},{d},{dist},{rtype},{status},{time_mins}),"
        "halt"
    )
    cmd = [SWIPL_CMD, "-q", "-s", PROLOG_FILE, "-g", goal_term]
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode == 0:
        print(
            "Road added in current Prolog runtime.\n"
            "To make it permanent, also add the same road/6 fact\n"
            "manually into prolog/roads_kb.pl and save the file."
        )
    else:
        print("Failed to add road.")
        if result.stderr:
            print("[Prolog error]", result.stderr.strip())


if __name__ == "__main__":
    main_menu()
