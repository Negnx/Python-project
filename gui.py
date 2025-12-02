"""
gui.py - Fixed GUI for Rural Road Path Finder
University of Technology Jamaica - AI Project
"""

import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
from typing import Optional, Dict
import sys
import os

# Import the main pathfinder module
try:
    # Set the path to the current directory so main.py can be imported
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))
    from main import RuralRoadPathFinder, PathFinderError
    # Import the visualizer module (assuming it's in the same directory)
    import graph_visualizer
except ImportError as e:
    messagebox.showerror("Setup Error", f"Failed to import a required module (main.py or graph_visualizer.py): {e}\nPlease ensure all files are in the same directory.")
    sys.exit(1)
except Exception as e:
    messagebox.showerror("Setup Error", f"An unexpected error occurred during import: {e}")
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
            
            # Call the new method to get network stats
            self.stats: Dict[str, int] = self.pathfinder.get_network_stats() 
            
            if not self.locations:
                raise PathFinderError("No locations found. Check 'roads_kb.pl' and Prolog setup.")
        except PathFinderError as e:
            messagebox.showerror("Initialization Error", f"PathFinder initialization failed:\n{str(e)}")
            self.root.destroy()
            return
        except Exception as e:
            messagebox.showerror("Initialization Error", f"An unexpected error occurred during initialization:\n{e}")
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
        left_panel.pack(side=tk.LEFT, fill=tk.Y, padx=(0, 5), pady=0)
        left_panel.config(width=450)
        
        # Create a canvas with scrollbar inside left_panel
        canvas = tk.Canvas(left_panel, bg='white', width=430)
        scrollbar = tk.Scrollbar(left_panel, orient="vertical", command=canvas.yview)
        
        # Frame to hold the widgets
        self.scrollable_frame = tk.Frame(canvas, bg='white')
        
        self.scrollable_frame.bind(
            "<Configure>",
            lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
        )
        
        # Draw the frame onto the canvas
        canvas.create_window((0, 0), window=self.scrollable_frame, anchor="nw", width=430)
        canvas.configure(yscrollcommand=scrollbar.set)
        
        # Pack canvas and scrollbar
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)

        # === LOCATIONS ===
        tk.Label(self.scrollable_frame, text="SELECT LOCATIONS", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        tk.Label(self.scrollable_frame, text="Starting Location:", 
                font=('Arial', 10), bg='white').pack(anchor=tk.W, padx=20)
        self.start_var = tk.StringVar()
        start_combo = ttk.Combobox(self.scrollable_frame, textvariable=self.start_var,
                                   values=self._format_locations(), 
                                   state='readonly', width=40, font=('Arial', 10))
        start_combo.pack(padx=20, pady=5, fill=tk.X)
        
        tk.Label(self.scrollable_frame, text="Destination:", 
                font=('Arial', 10), bg='white').pack(anchor=tk.W, padx=20, pady=(10,0))
        self.goal_var = tk.StringVar()
        goal_combo = ttk.Combobox(self.scrollable_frame, textvariable=self.goal_var,
                                  values=self._format_locations(), 
                                  state='readonly', width=40, font=('Arial', 10))
        goal_combo.pack(padx=20, pady=5, fill=tk.X)
        
        # === ALGORITHM ===
        tk.Label(self.scrollable_frame, text="SELECT ALGORITHM", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=(20,10))
        
        self.algo_var = tk.StringVar(value="shortest_distance")
        algorithms = [
            ("Shortest Distance (Dijkstra)", "shortest_distance"),
            ("Fastest Time", "fastest_time"),
            ("Fewest Hops (BFS)", "bfs"),
            ("Intelligent (A*)", "astar")
        ]
        
        for label, value in algorithms:
            tk.Radiobutton(self.scrollable_frame, text=label, variable=self.algo_var,
                          value=value, bg='white', font=('Arial', 10)).pack(anchor=tk.W, padx=30)
        
        # === CONSTRAINTS ===
        tk.Label(self.scrollable_frame, text="ROUTE CONSTRAINTS", 
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
            tk.Checkbutton(self.scrollable_frame, text=label, variable=var, 
                          bg='white', font=('Arial', 10)).pack(anchor=tk.W, padx=30)
            self.criteria_vars[value] = var
        
        # === BIG FIND ROUTE BUTTON ===
        find_button = tk.Button(self.scrollable_frame, 
                               text="🔍 FIND ROUTE",
                               command=self.find_route,
                               bg='#27ae60',
                               fg='white',
                               font=('Arial', 14, 'bold'),
                               pady=15,
                               cursor='hand2')
        find_button.pack(fill=tk.X, padx=20, pady=30)
        
        # GRAPH VISUALIZER BUTTON
        graph_button = tk.Button(self.scrollable_frame, 
                                 text="🗺️ OPEN GRAPH VIEW",
                                 command=self.launch_graph_view, 
                                 bg='#f39c12',
                                 fg='white',
                                 font=('Arial', 14, 'bold'),
                                 pady=10,
                                 cursor='hand2')
        graph_button.pack(fill=tk.X, padx=20, pady=(0, 10)) # Adjusted pady
        
        # NEW FIX: ADMIN CONTROLS BUTTON
        admin_button = tk.Button(self.scrollable_frame, 
                                 text="⚙️ ADMIN: ADD ROAD",
                                 command=self.show_admin_controls, 
                                 bg='#6c757d',
                                 fg='white',
                                 font=('Arial', 14, 'bold'),
                                 pady=10,
                                 cursor='hand2')
        admin_button.pack(fill=tk.X, padx=20, pady=(10, 30))
        
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

    def launch_graph_view(self):
        """Launches the NetworkGraphVisualizer in a new top-level window."""
        try:
            graph_root = tk.Toplevel(self.root)
            graph_visualizer.NetworkGraphVisualizer(graph_root)
        except ImportError:
            messagebox.showerror("Error", "Could not find 'graph_visualizer.py'. Ensure the file is present.")
        except Exception as e:
            messagebox.showerror("Graph Error", f"Failed to launch graph view: {e}")
            
    # NEW FIX: ADMIN CONTROLS METHODS
    def show_admin_controls(self):
        """Launches a dialog for adding a new road."""
        admin_window = tk.Toplevel(self.root)
        admin_window.title("Admin: Add New Road")
        admin_window.geometry("400x450")
        admin_window.transient(self.root) # Keeps it above the main window
        
        frame = tk.Frame(admin_window, padx=10, pady=10)
        frame.pack(fill=tk.BOTH, expand=True)

        tk.Label(frame, text="Add New Road (Session Only)", font=('Arial', 12, 'bold')).grid(row=0, column=0, columnspan=2, pady=10)

        # Labels and Entry Fields setup
        fields = [
            ("Source Location:", "source_var"),
            ("Destination Location:", "dest_var"),
            ("Distance (km):", "distance_var"),
            ("Road Type (e.g., paved):", "type_var"),
            ("Status (e.g., open):", "status_var"),
            ("Travel Time (minutes):", "time_var"),
        ]

        self.admin_vars = {}
        for i, (label_text, var_name) in enumerate(fields, start=1):
            tk.Label(frame, text=label_text).grid(row=i, column=0, sticky=tk.W, pady=5)
            
            var = tk.StringVar()
            entry = tk.Entry(frame, textvariable=var, width=30)
            entry.grid(row=i, column=1, sticky=tk.EW, padx=10, pady=5)
            self.admin_vars[var_name] = var
        
        # Admin action button
        add_button = tk.Button(frame, 
                               text="ADD ROAD",
                               command=lambda: self.add_road_action(admin_window),
                               bg='#3498db', fg='white', font=('Arial', 10, 'bold'), pady=8)
        add_button.grid(row=len(fields) + 1, column=0, columnspan=2, pady=20, sticky=tk.EW)
        
        admin_window.grab_set() # Make it modal
        self.root.wait_window(admin_window)
    
    def add_road_action(self, window):
        """Processes the input from the admin dialog and calls pathfinder.add_road."""
        try:
            source = self.admin_vars["source_var"].get().strip().lower().replace(' ', '_')
            dest = self.admin_vars["dest_var"].get().strip().lower().replace(' ', '_')
            
            # Use try-except for conversion to provide specific error messages
            try:
                distance = float(self.admin_vars["distance_var"].get().strip())
                time_mins = float(self.admin_vars["time_var"].get().strip())
            except ValueError:
                raise ValueError("Distance and Time must be numbers.")

            road_type = self.admin_vars["type_var"].get().strip().lower()
            status = self.admin_vars["status_var"].get().strip().lower()
            
            if not all([source, dest, road_type, status]):
                 raise ValueError("Source, Destination, Road Type, and Status cannot be empty.")
            
            # Call the main logic from PathFinder
            success = self.pathfinder.add_road(source, dest, distance, road_type, status, time_mins)
            
            if success:
                messagebox.showinfo("Success", f"Road '{source.title()} to {dest.title()}' added to the network for this session!")
                # Update stats and main message to reflect the change
                self.stats = self.pathfinder.get_network_stats() 
                self._display_welcome_message() 
                window.destroy()
            else:
                messagebox.showerror("Error", "Failed to add road. This may happen if Prolog rules were violated (e.g., trying to add the same road twice).")
                
        except ValueError as e:
            messagebox.showerror("Input Error", f"Invalid input: {e}")
        except Exception as e:
            messagebox.showerror("System Error", f"An unexpected error occurred: {e}")
            
    # ... (rest of the methods remain the same)
    def _display_welcome_message(self):
        """Display welcome message with network stats"""
        message = f"""
╔══════════════════════════════════════╗
║   Welcome to Path Finder System!    ║
╚══════════════════════════════════════╝

📊 NETWORK STATUS:
   - Locations: {self.stats.get('locations')}
   - Roads:     {self.stats.get('roads')}

📍 Select your starting location and 
   destination from the dropdowns.

🔧 Choose your preferred algorithm.

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