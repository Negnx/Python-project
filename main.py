"""
main.py - Jamaican Rural Road Path Finder
University of Technology Jamaica - AI Project

This module provides a command-line interface for the pathfinding system.
It communicates with Prolog to find optimal routes through Jamaica's rural road network.

Authors: [Your Group Members]
Date: November 2025
"""

import subprocess
import sys
import os
from typing import Optional, Tuple, List, Dict # Added Dict import

# Configuration
# Use a path relative to the current script's directory for robustness
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROLOG_FILE = r"C:\\Users\\mille\\Downloads\\VSCode\\Python project\\roads_kb.pl"

# --- IMPORTANT: UPDATE THIS PATH ---
# You MUST ensure this path points to your SWI-Prolog executable.
SWIPL_CMD = r"C:\Program Files\swipl\bin\swipl.exe"  # Update if swipl is not in PATH


class PathFinderError(Exception):
    """Custom exception for pathfinding errors"""
    pass


class RuralRoadPathFinder:
    """
    Main class for interacting with the Prolog pathfinding system.
    Handles query construction, execution, and result parsing.
    """
    
    def __init__(self, prolog_file: str = PROLOG_FILE):
        """
        Initialize the PathFinder with a Prolog knowledge base.
        
        Args:
            prolog_file: Path to the Prolog knowledge base file
        """
        self.prolog_file = prolog_file
        self._verify_prolog_installation()
        self._verify_kb_exists()
    
    def _verify_prolog_installation(self):
        """Check if SWI-Prolog is installed and accessible"""
        try:
            result = subprocess.run(
                [SWIPL_CMD, "--version"],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode != 0:
                raise PathFinderError("SWI-Prolog not found or not working properly")
        except FileNotFoundError:
            print("\n" + "="*70)
            print("ERROR: SWI-Prolog (swipl) not found!")
            print("="*70)
            print("Please install SWI-Prolog:")
            print("  - Windows: Download from https://www.swi-prolog.org/download/stable")
            print("  - Mac: brew install swi-prolog")
            print("  - Linux: sudo apt-get install swi-prolog")
            print("="*70 + "\n")
            sys.exit(1)
    
    def _verify_kb_exists(self):
        """Check if the Prolog knowledge base file exists"""
        if not os.path.exists(self.prolog_file):
            raise PathFinderError(
                f"Knowledge base file '{self.prolog_file}' not found!\n"
                f"Please ensure the file is in the current directory as main.py."
            )
    
    def find_route(self, algorithm: str, start: str, goal: str, 
                   criteria: List[str]) -> Optional[Tuple[List[str], float, float]]:
        """
        Find a route using the specified algorithm and criteria.
        
        Args:
            algorithm: Search algorithm (shortest_distance, fastest_time, bfs, astar)
            start: Starting location
            goal: Destination location
            criteria: List of avoidance criteria (e.g., ['avoid(unpaved)', 'avoid(closed)'])
        
        Returns:
            Tuple of (path_list, distance_km, time_minutes) or None if no route found
        """
        # Construct criteria list for Prolog
        criteria_terms = ",".join(criteria)
        criteria_list = f"[{criteria_terms}]" if criteria_terms else "[]"
        
        # Build Prolog query
        goal_term = (
            f"find_route({algorithm},{start},{goal},{criteria_list},Path,Distance,Time),"
            "write(Path),write('|'),write(Distance),write('|'),write(Time),nl"
        )
        
        # Execute Prolog query
        cmd = [SWIPL_CMD, "-q", "-s", self.prolog_file, "-g", goal_term, "-t", "halt"]
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30
            )
        except subprocess.TimeoutExpired:
            raise PathFinderError("Query timeout - route search took too long")
        
        # Check for errors
        if result.returncode != 0:
            if result.stderr:
                error_msg = result.stderr.strip()
                if "existence_error" in error_msg:
                    if 'coords' in error_msg or 'road' in error_msg:
                        raise PathFinderError(f"Location not found in network: {start} or {goal}")
                    else:
                        raise PathFinderError(f"Prolog runtime error (code {result.returncode}): {error_msg}")
            
            # If stdout is empty, it means 'find_route' failed (no path found)
            if not result.stdout.strip():
                 return None
            
            # Catch-all for non-zero return code
            raise PathFinderError(f"Prolog process failed with return code {result.returncode}")
        
        # Parse output
        if not result.stdout.strip():
            return None
        
        return self._parse_prolog_output(result.stdout.strip())
    
    def _parse_prolog_output(self, output: str) -> Tuple[List[str], float, float]:
        """
        Parse Prolog output into Python data structures.
        
        Args:
            output: Raw output from Prolog
        
        Returns:
            Tuple of (path_list, distance, time)
        """
        try:
            lines = output.strip().split('\n')
            line = lines[0].strip()
            
            # Split by delimiter
            parts = line.split('|')
            if len(parts) != 3:
                raise ValueError("Unexpected output format")
            
            # Parse path: [kingston,spanish_town,linstead] -> list
            path_str = parts[0].strip()
            path_str = path_str.strip('[]')
            
            # Handle empty list case
            if not path_str:
                path_list = []
            else:
                path_list = [loc.strip() for loc in path_str.split(',')]
            
            # Parse distance and time
            distance = float(parts[1].strip())
            time_minutes = float(parts[2].strip())
            
            return path_list, distance, time_minutes
            
        except (ValueError, IndexError) as e:
            raise PathFinderError(f"Failed to parse Prolog output: {output}\nError: {e}")

    
    def get_network_stats(self) -> Dict[str, int]:
        """
        Retrieve network statistics (number of locations and roads)
        from the knowledge base.
        """
        goal_term = (
            "get_network_stats(Locs, Roads),"
            "write(Locs),write('|'),write(Roads),nl,halt"
        )
        
        cmd = [SWIPL_CMD, "-q", "-s", self.prolog_file, "-g", goal_term]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            
            if result.returncode == 0 and result.stdout.strip():
                # Parse output: "24|48"
                parts = result.stdout.strip().split('|')
                if len(parts) == 2:
                    return {
                        'locations': int(parts[0].strip()),
                        'roads': int(parts[1].strip())
                    }
        except Exception as e:
            print(f"Warning: Failed to get network stats. Error: {e}", file=sys.stderr)
        
        # Return fallback stats if query fails
        # Use a list comprehension to count locations without relying on get_all_locations()
        # if initialization failed. For simplicity and robustness here, we'll call get_all_locations().
        return {
            'locations': len(self.get_all_locations()),
            'roads': 0 
        }

    def add_road(self, source: str, dest: str, distance: float,
                 road_type: str, status: str, time_mins: float) -> bool:
        """
        Add a new road to the network (runtime only).
        
        Args:
            source: Source location
            dest: Destination location
            distance: Distance in kilometers
            road_type: Type of road (paved, unpaved, etc.)
            status: Road status (open, closed, etc.)
            time_mins: Travel time in minutes
        
        Returns:
            True if successful, False otherwise
        """
        goal_term = (
            f"add_road({source},{dest},{distance},{road_type},{status},{time_mins}),"
            "halt"
        )
        
        cmd = [SWIPL_CMD, "-q", "-s", self.prolog_file, "-g", goal_term]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            return result.returncode == 0
        except Exception as e:
            print(f"Error adding road: {e}")
            return False
    
    def get_all_locations(self) -> List[str]:
        """
        Retrieve all locations from the knowledge base.
        
        Returns:
            List of location names
        """
        goal_term = (
            "get_all_locations(Locs),"
            "write(Locs),nl,halt"
        )
        
        cmd = [SWIPL_CMD, "-q", "-s", self.prolog_file, "-g", goal_term]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            if result.returncode == 0 and result.stdout.strip():
                # Parse list from Prolog output
                locs_str = result.stdout.strip().strip('[]')
                
                if locs_str:
                    locations = [loc.strip() for loc in locs_str.split(',')]
                    return sorted(locations)
                else:
                    return []
        except Exception:
            pass
        
        # Fallback to hardcoded list if query fails
        return self._get_default_locations()
    
    def _get_default_locations(self) -> List[str]:
        """Return default list of locations"""
        return sorted([
            'kingston', 'spanish_town', 'portmore', 'old_harbour',
            'may_pen', 'lionel_town', 'chapelton', 'mandeville',
            'christiana', 'porus', 'linstead', 'bog_walk', 'ewarton',
            'riversdale', 'ocho_rios', 'st_anns_bay', 'brown_town',
            'alexandria', 'runaway_bay', 'port_maria', 'annotto_bay',
            'gayle', 'santa_cruz', 'black_river'
        ])


def display_banner():
    """Display application banner"""
    print("\n" + "="*70)
    print(" "*15 + "RURAL ROAD PATH FINDER")
    print(" "*10 + "University of Technology Jamaica")
    print("="*70 + "\n")


def choose_algorithm() -> str:
    """
    Prompt user to select a pathfinding algorithm.
    
    Returns:
        Algorithm identifier string
    """
    print("\n--- Select Pathfinding Algorithm ---")
    print("1. Shortest Distance (Dijkstra's Algorithm)")
    print("2. Fastest Time (Time-optimized Dijkstra)")
    print("3. Fewest Hops (BFS - Breadth-First Search)")
    print("4. Intelligent Search (A* with GPS heuristics)")
    
    while True:
        choice = input("\nEnter your choice (1-4): ").strip()
        
        if choice == "1":
            return "shortest_distance"
        elif choice == "2":
            return "fastest_time"
        elif choice == "3":
            return "bfs"
        elif choice == "4":
            return "astar"
        else:
            print("Invalid choice. Please enter 1, 2, 3, or 4.")


def choose_criteria() -> List[str]:
    """
    Prompt user to select route criteria/constraints.
    
    Returns:
        List of criteria strings
    """
    criteria = []
    
    print("\n--- Select Route Criteria ---")
    print("(Press 'y' to apply each constraint, or Enter to skip)\n")
    
    constraints = [
        ("Avoid unpaved roads?", "avoid(unpaved)"),
        ("Avoid closed roads?", "avoid(closed)"),
        ("Avoid roads under repair?", "avoid(under_repair)"),
        ("Avoid seasonal roads?", "avoid(seasonal)"),
        ("Avoid roads with deep potholes?", "avoid(deep_potholes)"),
        ("Avoid roads with broken cisterns?", "avoid(broken_cisterns)"),
    ]
    
    for prompt, criterion in constraints:
        response = input(f"{prompt} (y/N): ").strip().lower()
        if response in ['y', 'yes']:
            criteria.append(criterion)
    
    if criteria:
        print(f"\nApplying {len(criteria)} constraint(s)")
    else:
        print("\nNo constraints applied")
    
    return criteria


def format_path(path: List[str]) -> str:
    """
    Format path list for display.
    
    Args:
        path: List of location names
    
    Returns:
        Formatted string representation
    """
    return " → ".join([loc.replace('_', ' ').title() for loc in path])


def display_route_results(path: List[str], distance: float, time: float):
    """
    Display route finding results in a formatted manner.
    
    Args:
        path: List of locations in the route
        distance: Total distance in kilometers
        time: Total time in minutes
    """
    print("\n" + "="*70)
    print(" "*25 + "ROUTE FOUND!")
    print("="*70)
    
    print(f"\n📍 Route ({len(path)} stops):")
    print(f"   {format_path(path)}")
    
    print(f"\n📏 Total Distance: {distance:.1f} km")
    print(f"⏱️  Estimated Time: {time:.0f} minutes ({time/60:.1f} hours)")
    
    print("\n" + "="*70 + "\n")


def find_route_interactive(pathfinder: RuralRoadPathFinder):
    """
    Interactive route finding session.
    
    Args:
        pathfinder: PathFinder instance
    """
    print("\n--- Find Route ---")
    
    # Get available locations
    locations = pathfinder.get_all_locations()
    
    if not locations:
        print("\n❌ Error: No locations found in the network.")
        return
        
    print(f"\nAvailable locations ({len(locations)}):")
    print("-" * 70)
    for i, loc in enumerate(locations, 1):
        print(f"  {i:2d}. {loc.replace('_', ' ').title()}", end="")
        if i % 3 == 0 or i == len(locations):
            print()
        else:
            print("\t", end="")
    print("-" * 70)
    
    # Get start and destination
    start = input("Enter starting location: ").strip().lower().replace(' ', '_')
    goal = input("Enter destination: ").strip().lower().replace(' ', '_')
    
    if start not in locations or goal not in locations:
        print("\n❌ Error: One or both locations not found in the network.")
        print("Please check spelling.")
        return
    
    if start == goal:
        print("\n❌ Error: Start and destination are the same!")
        return
    
    # Choose algorithm and criteria
    algorithm = choose_algorithm()
    criteria = choose_criteria()
    
    # Find route
    print("\n🔍 Searching for route...")
    
    try:
        result = pathfinder.find_route(algorithm, start, goal, criteria)
        
        if result is None:
            print("\n❌ No route found!")
            print("   Try removing some constraints or using different locations.")
        else:
            path, distance, time = result
            display_route_results(path, distance, time)
            
    except PathFinderError as e:
        print(f"\n❌ Error: {e}")
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")


def add_road_interactive(pathfinder: RuralRoadPathFinder):
    """
    Interactive road addition session (admin function).
    
    Args:
        pathfinder: PathFinder instance
    """
    print("\n--- Add Road (Administrator) ---")
    print("⚠️  Note: This adds the road for the current session only.")
    print("    To persist permanently, edit roads_kb.pl manually.\n")
    
    try:
        source = input("Source location: ").strip().lower().replace(' ', '_')
        dest = input("Destination location: ").strip().lower().replace(' ', '_')
        distance = float(input("Distance (km): ").strip())
        
        print("\nRoad types: paved, unpaved, deep_potholes, broken_cisterns")
        road_type = input("Road type: ").strip().lower()
        
        print("\nStatus options: open, closed, under_repair, seasonal")
        status = input("Status: ").strip().lower()
        
        time_mins = float(input("Travel time (minutes): ").strip())
        
        # Add the road
        success = pathfinder.add_road(source, dest, distance, road_type, status, time_mins)
        
        if success:
            print("\n✅ Road added successfully!")
            print(f"   {source.title()} → {dest.title()}: {distance}km, {road_type}, {status}")
        else:
            print("\n❌ Failed to add road. Please check your input.")
            
    except ValueError:
        print("\n❌ Invalid input. Distance and time must be numbers.")
    except Exception as e:
        print(f"\n❌ Error: {e}")


def main_menu():
    """Main application loop with menu interface"""
    display_banner()
    
    # Initialize pathfinder
    try:
        pathfinder = RuralRoadPathFinder()
    except PathFinderError as e:
        print(f"❌ Initialization Error: {e}")
        return
    
    while True:
        print("\n" + "-"*70)
        print("MAIN MENU")
        print("-"*70)
        print("1. Find Route")
        print("2. Add Road (Admin)")
        print("3. View All Locations/Stats") # Updated menu option name
        print("4. Launch GUI (if available)")
        print("5. Exit")
        print("-"*70)
        
        choice = input("\nSelect an option (1-5): ").strip()
        
        if choice == "1":
            find_route_interactive(pathfinder)
            
        elif choice == "2":
            add_road_interactive(pathfinder)
            
        elif choice == "3":
            # Display stats using the new method
            locations = pathfinder.get_all_locations()
            stats = pathfinder.get_network_stats()
            print(f"\n📍 Network Statistics:")
            print(f"   - Total Locations: {stats.get('locations')}")
            print(f"   - Total Roads: {stats.get('roads')}")
            print("-" * 70)
            print(f"📍 Available Locations ({len(locations)}):")
            print("-" * 70)
            for i, loc in enumerate(locations, 1):
                print(f"  {i:2d}. {loc.replace('_', ' ').title()}")
            print("-" * 70)
            
        elif choice == "4":
            print("\n🚀 Launching GUI...")
            try:
                import gui
                gui.launch_gui()
            except ImportError:
                print("❌ GUI module not found. Run 'gui.py' separately.")
            except Exception as e:
                print(f"❌ Error launching GUI: {e}")
                
        elif choice == "5":
            print("\n👋 Thank you for using Rural Road Path Finder!")
            print("="*70 + "\n")
            break
            
        else:
            print("\n❌ Invalid option. Please select 1-5.")


if __name__ == "__main__":
    try:
        main_menu()
    except KeyboardInterrupt:
        print("\n\n👋 Program interrupted by user. Goodbye!")
    except Exception as e:
        print(f"\n❌ Fatal error: {e}")
        sys.exit(1)