"""
gui.py - Fixed GUI for Rural Road Path Finder
University of Technology Jamaica - AI Project
"""

import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
from typing import Optional
import sys

# Import the main pathfinder module
try:
    from main import RuralRoadPathFinder, PathFinderError
except ImportError:
    print("Error: main.py not found. Please ensure main.py is in the same directory.")
    sys.exit(1)


class PathFinderGUI:
    """Fixed GUI for the Rural Road Path Finder."""
    
    def __init__(self, root):
        self.root = root
        self.root.title("Jamaican Rural Road Path Finder")
        self.root.geometry("1200x800")
        
        # Initialize pathfinder
        try:
            self.pathfinder = RuralRoadPathFinder()
            self.locations = self.pathfinder.get_all_locations()
        except PathFinderError as e:
            messagebox.showerror("Initialization Error", str(e))
            self.root.destroy()
            return
        
        self.create_gui()
    
    def create_gui(self):
        """Create the GUI components"""
        
        # Header
        header = tk.Label(self.root, 
                         text="🚗 Jamaican Rural Road Path Finder",
                         font=('Arial', 18, 'bold'),
                         bg='#3498db',
                         fg='white',
                         pady=15)
        header.pack(fill=tk.X)
        
        # Main container
        main_container = tk.Frame(self.root)
        main_container.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Left panel (inputs)
        left_panel = tk.Frame(main_container, bg='white', relief=tk.RIDGE, bd=2)
        left_panel.pack(side=tk.LEFT, fill=tk.BOTH, padx=(0, 5), pady=0)
        left_panel.config(width=450)
        
        # Create a canvas with scrollbar inside left_panel
        canvas = tk.Canvas(left_panel, bg='white', width=430)
        scrollbar = tk.Scrollbar(left_panel, orient="vertical", command=canvas.yview)
        scrollable_frame = tk.Frame(canvas, bg='white')
        
        scrollable_frame.bind(
            "<Configure>",
            lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
        )
        
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw", width=430)
        canvas.configure(yscrollcommand=scrollbar.set)
        
        # === LOCATIONS ===
        tk.Label(scrollable_frame, text="SELECT LOCATIONS", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        tk.Label(scrollable_frame, text="Starting Location:", 
                font=('Arial', 10), bg='white').pack(anchor=tk.W, padx=20)
        self.start_var = tk.StringVar()
        start_combo = ttk.Combobox(scrollable_frame, textvariable=self.start_var,
                                   values=self._format_locations(), 
                                   state='readonly', width=40, font=('Arial', 10))
        start_combo.pack(padx=20, pady=5, fill=tk.X)
        
        tk.Label(scrollable_frame, text="Destination:", 
                font=('Arial', 10), bg='white').pack(anchor=tk.W, padx=20, pady=(10,0))
        self.goal_var = tk.StringVar()
        goal_combo = ttk.Combobox(scrollable_frame, textvariable=self.goal_var,
                                  values=self._format_locations(), 
                                  state='readonly', width=40, font=('Arial', 10))
        goal_combo.pack(padx=20, pady=5, fill=tk.X)
        
        # === ALGORITHM ===
        tk.Label(scrollable_frame, text="SELECT ALGORITHM", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=(20,10))
        
        self.algo_var = tk.StringVar(value="shortest_distance")
        algorithms = [
            ("Shortest Distance (Dijkstra)", "shortest_distance"),
            ("Fastest Time", "fastest_time"),
            ("Fewest Hops (BFS)", "bfs"),
            ("Intelligent (A*)", "astar")
        ]
        
        for label, value in algorithms:
            tk.Radiobutton(scrollable_frame, text=label, variable=self.algo_var,
                          value=value, bg='white', font=('Arial', 10)).pack(anchor=tk.W, padx=30)
        
        # === CONSTRAINTS ===
        tk.Label(scrollable_frame, text="ROUTE CONSTRAINTS", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=(20,10))
        
        self.criteria_vars = {}
        constraints = [
            ("Avoid unpaved roads", "avoid(unpaved)"),
            ("Avoid closed roads", "avoid(closed)"),
            ("Avoid roads under repair", "avoid(under_repair)"),
            ("Avoid seasonal roads", "avoid(seasonal)"),
            ("Avoid deep potholes", "avoid(deep_potholes)"),
            ("Avoid broken cisterns", "avoid(broken_cisterns)")
        ]
        
        for label, value in constraints:
            var = tk.BooleanVar()
            tk.Checkbutton(scrollable_frame, text=label, variable=var, 
                          bg='white', font=('Arial', 10)).pack(anchor=tk.W, padx=30)
            self.criteria_vars[value] = var
        
        # === BIG FIND ROUTE BUTTON ===
        find_button = tk.Button(scrollable_frame, 
                               text="🔍 FIND ROUTE",
                               command=self.find_route,
                               bg='#27ae60',
                               fg='white',
                               font=('Arial', 14, 'bold'),
                               pady=15,
                               cursor='hand2')
        find_button.pack(fill=tk.X, padx=20, pady=30)
        
        # Pack canvas and scrollbar
        canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # Right panel (results)
        right_panel = tk.Frame(main_container, bg='white', relief=tk.RIDGE, bd=2)
        right_panel.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=(5, 0), pady=0)
        
        tk.Label(right_panel, text="RESULTS", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        self.results_text = scrolledtext.ScrolledText(right_panel, 
                                                      wrap=tk.WORD,
                                                      font=('Courier', 10),
                                                      width=50,
                                                      height=35)
        self.results_text.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Welcome message
        self._display_welcome_message()
        
        # Footer
        footer = tk.Label(self.root, 
                         text="© 2025 UTech Jamaica | AI Project",
                         font=('Arial', 9),
                         bg='#ecf0f1',
                         pady=10)
        footer.pack(fill=tk.X, side=tk.BOTTOM)
    
    def _format_locations(self):
        """Format location names for display"""
        return [loc.replace('_', ' ').title() for loc in self.locations]
    
    def _get_selected_criteria(self):
        """Get list of selected criteria"""
        return [criterion for criterion, var in self.criteria_vars.items() if var.get()]
    
    def _display_welcome_message(self):
        """Display welcome message"""
        message = """
╔══════════════════════════════════════╗
║   Welcome to Path Finder System!    ║
╚══════════════════════════════════════╝

📍 Select your starting location and 
   destination from the dropdowns.

🔧 Choose your preferred algorithm:
   • Shortest Distance (Dijkstra)
   • Fastest Time
   • Fewest Hops (BFS)
   • Intelligent Search (A*)

⚙️  Apply route constraints as needed.

🔍 Click the BIG GREEN "FIND ROUTE" 
   button to begin!

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        """
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', message)
    
    def find_route(self):
        """Handle route finding request"""
        start_formatted = self.start_var.get()
        goal_formatted = self.goal_var.get()
        
        if not start_formatted or not goal_formatted:
            messagebox.showwarning("Input Required", 
                                 "Please select both start and destination locations.")
            return
        
        start = start_formatted.lower().replace(' ', '_')
        goal = goal_formatted.lower().replace(' ', '_')
        
        if start == goal:
            messagebox.showwarning("Invalid Selection", 
                                 "Start and destination cannot be the same!")
            return
        
        algorithm = self.algo_var.get()
        criteria = self._get_selected_criteria()
        
        # Show searching message
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', "🔍 Searching for route...\n\n")
        self.root.update()
        
        try:
            result = self.pathfinder.find_route(algorithm, start, goal, criteria)
            
            if result is None:
                self._display_no_route()
            else:
                path, distance, time = result
                self._display_route_result(path, distance, time, algorithm)
                
        except PathFinderError as e:
            self._display_error(str(e))
        except Exception as e:
            self._display_error(f"Unexpected error: {e}")
    
    def _display_route_result(self, path, distance, time, algorithm):
        """Display route results"""
        formatted_path = [loc.replace('_', ' ').title() for loc in path]
        
        result = f"""
╔══════════════════════════════════════╗
║         ROUTE FOUND! ✓               ║
╚══════════════════════════════════════╝

🗺️  Algorithm: {self._get_algorithm_name(algorithm)}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📍 ROUTE ({len(path)} locations):

"""
        
        for i, location in enumerate(formatted_path, 1):
            if i == 1:
                result += f"   {i}. 🏁 {location}\n"
            elif i == len(formatted_path):
                result += f"   {i}. 🎯 {location}\n"
            else:
                result += f"   {i}. 📌 {location}\n"
        
        result += f"""
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📊 STATISTICS:

   📏 Total Distance:  {distance:.1f} km
   ⏱️  Estimated Time:  {time:.0f} minutes
                       ({time/60:.1f} hours)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✅ Route calculation complete!
        """
        
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', result)
    
    def _display_no_route(self):
        """Display no route found message"""
        message = """
╔══════════════════════════════════════╗
║      NO ROUTE FOUND ✗                ║
╚══════════════════════════════════════╝

❌ No valid route exists with the current
   constraints.

💡 SUGGESTIONS:

   • Try removing some constraints
   • Choose different locations
   • Use a different algorithm
   • Check if roads are closed

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        """
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', message)
    
    def _display_error(self, error_msg):
        """Display error message"""
        message = f"""
╔══════════════════════════════════════╗
║           ERROR ✗                    ║
╚══════════════════════════════════════╝

❌ An error occurred:

   {error_msg}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        """
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', message)
    
    def _get_algorithm_name(self, algo_key):
        """Get display name for algorithm"""
        names = {
            'shortest_distance': 'Shortest Distance (Dijkstra)',
            'fastest_time': 'Fastest Time',
            'bfs': 'Fewest Hops (BFS)',
            'astar': 'Intelligent Search (A*)'
        }
        return names.get(algo_key, algo_key)


def launch_gui():
    """Launch the GUI application"""
    root = tk.Tk()
    app = PathFinderGUI(root)
    root.mainloop()


if __name__ == "__main__":
    launch_gui()