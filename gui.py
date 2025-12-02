"""
gui.py - Enhanced GUI for Rural Road Path Finder
University of Technology Jamaica - AI Project
Includes graph visualization and persistent storage
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
    """Enhanced GUI for the Rural Road Path Finder."""
    
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
        
        self.last_result = None  # Store last route for visualization
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
        
        # === BUTTONS ===
        button_frame = tk.Frame(scrollable_frame, bg='white')
        button_frame.pack(fill=tk.X, padx=20, pady=20)
        
        find_button = tk.Button(button_frame, 
                               text="🔍 FIND ROUTE",
                               command=self.find_route,
                               bg='#27ae60',
                               fg='white',
                               font=('Arial', 12, 'bold'),
                               pady=10,
                               cursor='hand2')
        find_button.pack(fill=tk.X, pady=(0, 10))
        
        graph_button = tk.Button(button_frame, 
                                text="🗺️ VIEW NETWORK MAP",
                                command=self.open_graph_view,
                                bg='#3498db',
                                fg='white',
                                font=('Arial', 11, 'bold'),
                                pady=8,
                                cursor='hand2')
        graph_button.pack(fill=tk.X, pady=(0, 10))
        
        admin_button = tk.Button(button_frame, 
                                text="⚙️ ADMIN PANEL",
                                command=self.open_admin_panel,
                                bg='#e67e22',
                                fg='white',
                                font=('Arial', 11, 'bold'),
                                pady=8,
                                cursor='hand2')
        admin_button.pack(fill=tk.X)
        
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
        stats = self.pathfinder.get_network_stats()
        message = f"""
╔═══════════════════════════════════════╗
║   Welcome to Path Finder System!    ║
╚═══════════════════════════════════════╝

📊 NETWORK STATISTICS:
   • Locations: {stats['locations']}
   • Roads: {stats['roads']}

🔍 SELECT YOUR ROUTE:
   1. Choose start and destination
   2. Select pathfinding algorithm
   3. Apply route constraints
   4. Click "FIND ROUTE"

🗺️  VIEW NETWORK MAP:
   See visual representation of the
   entire road network as a graph

⚙️  ADMIN PANEL:
   Add new roads or update status
   (changes are saved permanently)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
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
                self.last_result = None
            else:
                path, distance, time = result
                self.last_result = (path, distance, time)
                self._display_route_result(path, distance, time, algorithm)
                
        except PathFinderError as e:
            self._display_error(str(e))
            self.last_result = None
        except Exception as e:
            self._display_error(f"Unexpected error: {e}")
            self.last_result = None
    
    def _display_route_result(self, path, distance, time, algorithm):
        """Display route results"""
        formatted_path = [loc.replace('_', ' ').title() for loc in path]
        
        result = f"""
╔═══════════════════════════════════════╗
║         ROUTE FOUND! ✓               ║
╚═══════════════════════════════════════╝

🗺️  Algorithm: {self._get_algorithm_name(algorithm)}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📍 ROUTE ({len(path)} locations):

"""
        
        for i, location in enumerate(formatted_path, 1):
            if i == 1:
                result += f"   {i}. 🚗 {location}\n"
            elif i == len(formatted_path):
                result += f"   {i}. 🎯 {location}\n"
            else:
                result += f"   {i}. 📌 {location}\n"
        
        result += f"""
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📊 STATISTICS:

   📏 Total Distance:  {distance:.1f} km
   ⏱️  Estimated Time:  {time:.0f} minutes
                       ({time/60:.1f} hours)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✅ Route calculation complete!

💡 TIP: Click "VIEW NETWORK MAP" to see
   this route visualized on the graph!
        """
        
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', result)
    
    def _display_no_route(self):
        """Display no route found message"""
        message = """
╔═══════════════════════════════════════╗
║      NO ROUTE FOUND ✗                ║
╚═══════════════════════════════════════╝

❌ No valid route exists with the current
   constraints.

💡 SUGGESTIONS:

   • Try removing some constraints
   • Choose different locations
   • Use a different algorithm
   • Check if roads are closed

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        """
        self.results_text.delete('1.0', tk.END)
        self.results_text.insert('1.0', message)
    
    def _display_error(self, error_msg):
        """Display error message"""
        message = f"""
╔═══════════════════════════════════════╗
║           ERROR ✗                    ║
╚═══════════════════════════════════════╝

❌ An error occurred:

   {error_msg}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
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
    
    def open_graph_view(self):
        """Open the network graph visualizer"""
        try:
            import graph_visualizer
            
            # Create new window
            graph_window = tk.Toplevel(self.root)
            visualizer = graph_visualizer.NetworkGraphVisualizer(graph_window)
            
            # If we have a last result, highlight it
            if self.last_result:
                path, _, _ = self.last_result
                visualizer.highlight_path(path)
                
        except ImportError:
            messagebox.showerror("Error", 
                               "graph_visualizer.py not found.\n"
                               "Please ensure it's in the same directory.")
        except Exception as e:
            messagebox.showerror("Error", f"Failed to open graph view: {e}")
    
    def open_admin_panel(self):
        """Open admin panel for adding/updating roads"""
        admin_window = tk.Toplevel(self.root)
        admin_window.title("Administrator Panel")
        admin_window.geometry("600x500")
        
        # Header
        header = tk.Label(admin_window, 
                         text="⚙️ Administrator Panel",
                         font=('Arial', 16, 'bold'),
                         bg='#e67e22',
                         fg='white',
                         pady=10)
        header.pack(fill=tk.X)
        
        # Notebook for tabs
        notebook = ttk.Notebook(admin_window)
        notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Add Road Tab
        add_frame = tk.Frame(notebook, bg='white')
        notebook.add(add_frame, text="Add Road")
        
        self._create_add_road_form(add_frame)
        
        # Update Status Tab
        update_frame = tk.Frame(notebook, bg='white')
        notebook.add(update_frame, text="Update Status")
        
        self._create_update_status_form(update_frame)
    
    def _create_add_road_form(self, parent):
        """Create form for adding roads"""
        tk.Label(parent, text="Add New Road to Network", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        form_frame = tk.Frame(parent, bg='white')
        form_frame.pack(fill=tk.BOTH, expand=True, padx=20, pady=10)
        
        # Source
        tk.Label(form_frame, text="Source Location:", bg='white').grid(row=0, column=0, sticky=tk.W, pady=5)
        source_var = tk.StringVar()
        source_combo = ttk.Combobox(form_frame, textvariable=source_var,
                                   values=self._format_locations(), width=30)
        source_combo.grid(row=0, column=1, pady=5, padx=10)
        
        # Destination
        tk.Label(form_frame, text="Destination:", bg='white').grid(row=1, column=0, sticky=tk.W, pady=5)
        dest_var = tk.StringVar()
        dest_combo = ttk.Combobox(form_frame, textvariable=dest_var,
                                 values=self._format_locations(), width=30)
        dest_combo.grid(row=1, column=1, pady=5, padx=10)
        
        # Distance
        tk.Label(form_frame, text="Distance (km):", bg='white').grid(row=2, column=0, sticky=tk.W, pady=5)
        distance_var = tk.StringVar()
        distance_entry = tk.Entry(form_frame, textvariable=distance_var, width=32)
        distance_entry.grid(row=2, column=1, pady=5, padx=10)
        
        # Road Type
        tk.Label(form_frame, text="Road Type:", bg='white').grid(row=3, column=0, sticky=tk.W, pady=5)
        type_var = tk.StringVar()
        type_combo = ttk.Combobox(form_frame, textvariable=type_var,
                                 values=['paved', 'unpaved', 'deep_potholes', 'broken_cisterns'],
                                 width=30)
        type_combo.grid(row=3, column=1, pady=5, padx=10)
        
        # Status
        tk.Label(form_frame, text="Status:", bg='white').grid(row=4, column=0, sticky=tk.W, pady=5)
        status_var = tk.StringVar()
        status_combo = ttk.Combobox(form_frame, textvariable=status_var,
                                   values=['open', 'closed', 'under_repair', 'seasonal'],
                                   width=30)
        status_combo.grid(row=4, column=1, pady=5, padx=10)
        
        # Time
        tk.Label(form_frame, text="Travel Time (min):", bg='white').grid(row=5, column=0, sticky=tk.W, pady=5)
        time_var = tk.StringVar()
        time_entry = tk.Entry(form_frame, textvariable=time_var, width=32)
        time_entry.grid(row=5, column=1, pady=5, padx=10)
        
        # Add button
        def add_road():
            try:
                source = source_var.get().lower().replace(' ', '_')
                dest = dest_var.get().lower().replace(' ', '_')
                distance = float(distance_var.get())
                road_type = type_var.get()
                status = status_var.get()
                time_mins = float(time_var.get())
                
                if not all([source, dest, road_type, status]):
                    messagebox.showwarning("Input Required", "Please fill all fields")
                    return
                
                success = self.pathfinder.add_road_persistent(
                    source, dest, distance, road_type, status, time_mins
                )
                
                if success:
                    messagebox.showinfo("Success", "Road added and saved permanently!")
                    # Refresh locations
                    self.locations = self.pathfinder.get_all_locations()
                else:
                    messagebox.showerror("Error", "Failed to add road")
                    
            except ValueError:
                messagebox.showerror("Error", "Invalid number format")
            except Exception as e:
                messagebox.showerror("Error", str(e))
        
        add_btn = tk.Button(form_frame, text="Add Road", command=add_road,
                           bg='#27ae60', fg='white', font=('Arial', 11, 'bold'),
                           pady=8, cursor='hand2')
        add_btn.grid(row=6, column=0, columnspan=2, pady=20)
    
    def _create_update_status_form(self, parent):
        """Create form for updating road status"""
        tk.Label(parent, text="Update Road Status", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        form_frame = tk.Frame(parent, bg='white')
        form_frame.pack(fill=tk.BOTH, expand=True, padx=20, pady=10)
        
        # Source
        tk.Label(form_frame, text="Source Location:", bg='white').grid(row=0, column=0, sticky=tk.W, pady=5)
        source_var = tk.StringVar()
        source_combo = ttk.Combobox(form_frame, textvariable=source_var,
                                   values=self._format_locations(), width=30)
        source_combo.grid(row=0, column=1, pady=5, padx=10)
        
        # Destination
        tk.Label(form_frame, text="Destination:", bg='white').grid(row=1, column=0, sticky=tk.W, pady=5)
        dest_var = tk.StringVar()
        dest_combo = ttk.Combobox(form_frame, textvariable=dest_var,
                                 values=self._format_locations(), width=30)
        dest_combo.grid(row=1, column=1, pady=5, padx=10)
        
        # New Status
        tk.Label(form_frame, text="New Status:", bg='white').grid(row=2, column=0, sticky=tk.W, pady=5)
        status_var = tk.StringVar()
        status_combo = ttk.Combobox(form_frame, textvariable=status_var,
                                   values=['open', 'closed', 'under_repair', 'seasonal'],
                                   width=30)
        status_combo.grid(row=2, column=1, pady=5, padx=10)
        
        # Update button
        def update_status():
            try:
                source = source_var.get().lower().replace(' ', '_')
                dest = dest_var.get().lower().replace(' ', '_')
                status = status_var.get()
                
                if not all([source, dest, status]):
                    messagebox.showwarning("Input Required", "Please fill all fields")
                    return
                
                success = self.pathfinder.update_road_status_persistent(source, dest, status)
                
                if success:
                    messagebox.showinfo("Success", "Road status updated and saved!")
                else:
                    messagebox.showerror("Error", "Failed to update status")
                    
            except Exception as e:
                messagebox.showerror("Error", str(e))
        
        update_btn = tk.Button(form_frame, text="Update Status", command=update_status,
                              bg='#3498db', fg='white', font=('Arial', 11, 'bold'),
                              pady=8, cursor='hand2')
        update_btn.grid(row=3, column=0, columnspan=2, pady=20)


def launch_gui():
    """Launch the GUI application"""
    root = tk.Tk()
    app = PathFinderGUI(root)
    root.mainloop()


if __name__ == "__main__":
    launch_gui()