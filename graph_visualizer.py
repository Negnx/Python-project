"""
graph_visualizer.py - Visual Graph Representation
University of Technology Jamaica - AI Project

Creates a visual representation of the road network as a graph.
"""

import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
import math
import re


class NetworkGraphVisualizer:
    """Visualizes the road network as an interactive graph"""
    
    def __init__(self, root, prolog_file= r"C:\\Users\\mille\\Downloads\\VSCode\\Python project\\roads_kb.pl"):
        self.root = root
        self.root.title("Jamaican Rural Road Network - Graph View")
        self.root.geometry("1400x900")
        
        self.prolog_file = prolog_file
        self.nodes = {}  # {location: (x, y)}
        self.edges = []  # [(source, dest, distance, type, status, time)]
        self.coords_data = {}  # {location: (lat, lon)}
        
        self.selected_path = []
        self.load_network_data()
        self.create_gui()
        
    def load_network_data(self):
        """Load network data from Prolog file"""
        try:
            with open(self.prolog_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Extract coordinates
            coord_pattern = r'coords\((\w+),\s*([-\d.]+),\s*([-\d.]+)\)'
            for match in re.finditer(coord_pattern, content):
                location = match.group(1)
                lat = float(match.group(2))
                lon = float(match.group(3))
                self.coords_data[location] = (lat, lon)
            
            # Extract roads
            road_pattern = r'road\((\w+),\s*(\w+),\s*([\d.]+),\s*(\w+),\s*(\w+),\s*([\d.]+)\)'
            for match in re.finditer(road_pattern, content):
                source = match.group(1)
                dest = match.group(2)
                distance = float(match.group(3))
                road_type = match.group(4)
                status = match.group(5)
                time = float(match.group(6))
                self.edges.append((source, dest, distance, road_type, status, time))
            
            # Calculate node positions based on GPS coordinates
            self.calculate_node_positions()
            
        except FileNotFoundError:
            messagebox.showerror("Error", f"Cannot find {self.prolog_file}")
            self.root.destroy()
        except Exception as e:
            messagebox.showerror("Error", f"Failed to load network: {e}")
    
    def calculate_node_positions(self):
        """Convert GPS coordinates to canvas positions"""
        if not self.coords_data:
            return
        
        # Find bounds
        lats = [lat for lat, lon in self.coords_data.values()]
        lons = [lon for lat, lon in self.coords_data.values()]
        
        min_lat, max_lat = min(lats), max(lats)
        min_lon, max_lon = min(lons), max(lons)
        
        # Canvas dimensions (with padding)
        canvas_width = 1100
        canvas_height = 700
        padding = 80
        
        # Scale to canvas
        for location, (lat, lon) in self.coords_data.items():
            # Normalize to 0-1
            x_norm = (lon - min_lon) / (max_lon - min_lon) if max_lon != min_lon else 0.5
            y_norm = (lat - min_lat) / (max_lat - min_lat) if max_lat != min_lat else 0.5
            
            # Scale to canvas (flip Y because canvas Y increases downward)
            x = padding + x_norm * (canvas_width - 2 * padding)
            y = canvas_height - (padding + y_norm * (canvas_height - 2 * padding))
            
            self.nodes[location] = (x, y)
    
    def create_gui(self):
        """Create the GUI components"""
        
        # Title
        title = tk.Label(self.root, 
                        text="🗺️ Jamaican Rural Road Network Map",
                        font=('Arial', 16, 'bold'),
                        bg='#2c3e50',
                        fg='white',
                        pady=10)
        title.pack(fill=tk.X)
        
        # Main container
        main_container = tk.Frame(self.root)
        main_container.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Left panel - Canvas
        left_panel = tk.Frame(main_container)
        left_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=(0, 5))
        
        # Canvas with scrollbars
        self.canvas = tk.Canvas(left_panel, bg='#ecf0f1', width=1100, height=700)
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
        # Right panel - Legend and Info
        right_panel = tk.Frame(main_container, bg='white', relief=tk.RIDGE, bd=2)
        right_panel.pack(side=tk.RIGHT, fill=tk.BOTH, padx=(5, 0))
        right_panel.config(width=250)
        
        # Legend
        tk.Label(right_panel, text="LEGEND", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        legend_frame = tk.Frame(right_panel, bg='white')
        legend_frame.pack(fill=tk.BOTH, padx=10)
        
        # Road Type Legend
        tk.Label(legend_frame, text="Road Types:", 
                font=('Arial', 10, 'bold'), bg='white').pack(anchor=tk.W, pady=(5,2))
        
        road_types = [
            ("Paved", "#27ae60", "solid"),
            ("Unpaved", "#f39c12", "dashed"),
            ("Deep Potholes", "#e74c3c", "dotted"),
            ("Broken Cisterns", "#9b59b6", "dashdot")
        ]
        
        for label, color, _ in road_types:
            frame = tk.Frame(legend_frame, bg='white')
            frame.pack(anchor=tk.W, pady=2)
            
            canvas = tk.Canvas(frame, width=30, height=3, bg='white', 
                             highlightthickness=0)
            canvas.pack(side=tk.LEFT, padx=(10, 5))
            canvas.create_line(0, 1, 30, 1, fill=color, width=3)
            
            tk.Label(frame, text=label, bg='white', 
                    font=('Arial', 9)).pack(side=tk.LEFT)
        
        # Road Status Legend
        tk.Label(legend_frame, text="\nRoad Status:", 
                font=('Arial', 10, 'bold'), bg='white').pack(anchor=tk.W, pady=(10,2))
        
        status_types = [
            ("🟢 Open", "#27ae60"),
            ("🔴 Closed", "#e74c3c"),
            ("🟡 Under Repair", "#f39c12"),
            ("🟠 Seasonal", "#e67e22")
        ]
        
        for label, color in status_types:
            frame = tk.Frame(legend_frame, bg='white')
            frame.pack(anchor=tk.W, pady=2)
            tk.Label(frame, text=label, bg='white', 
                    font=('Arial', 9)).pack(side=tk.LEFT, padx=10)
        
        # Node Legend
        tk.Label(legend_frame, text="\nLocations:", 
                font=('Arial', 10, 'bold'), bg='white').pack(anchor=tk.W, pady=(10,2))
        
        tk.Label(legend_frame, text="⚪ Village/Town", bg='white',
                font=('Arial', 9)).pack(anchor=tk.W, padx=10, pady=2)
        tk.Label(legend_frame, text="🔵 Selected Path", bg='white',
                font=('Arial', 9)).pack(anchor=tk.W, padx=10, pady=2)
        
        # Statistics
        tk.Label(right_panel, text="\nSTATISTICS", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        stats_frame = tk.Frame(right_panel, bg='white')
        stats_frame.pack(fill=tk.BOTH, padx=10)
        
        tk.Label(stats_frame, 
                text=f"Total Locations: {len(self.nodes)}", 
                bg='white', font=('Arial', 9)).pack(anchor=tk.W, pady=2)
        tk.Label(stats_frame, 
                text=f"Total Roads: {len(self.edges)}", 
                bg='white', font=('Arial', 9)).pack(anchor=tk.W, pady=2)
        
        # Count road types
        paved = sum(1 for e in self.edges if e[3] == 'paved')
        unpaved = sum(1 for e in self.edges if e[3] == 'unpaved')
        
        tk.Label(stats_frame, 
                text=f"Paved Roads: {paved}", 
                bg='white', font=('Arial', 9)).pack(anchor=tk.W, pady=2)
        tk.Label(stats_frame, 
                text=f"Unpaved Roads: {unpaved}", 
                bg='white', font=('Arial', 9)).pack(anchor=tk.W, pady=2)
        
        # Info panel
        tk.Label(right_panel, text="\nINFORMATION", 
                font=('Arial', 12, 'bold'), bg='white').pack(pady=10)
        
        self.info_text = scrolledtext.ScrolledText(right_panel, 
                                                   wrap=tk.WORD,
                                                   font=('Courier', 8),
                                                   width=30,
                                                   height=15)
        self.info_text.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)
        self.info_text.insert('1.0', "Click on nodes or edges\nfor more information.")
        
        # Draw the network
        self.draw_network()
        
        # Bind click events
        self.canvas.bind("<Button-1>", self.on_canvas_click)
    
    def draw_network(self):
        """Draw the complete network on canvas"""
        self.canvas.delete("all")
        
        # Draw edges first (so they appear behind nodes)
        for source, dest, distance, road_type, status, time in self.edges:
            if source in self.nodes and dest in self.nodes:
                self.draw_edge(source, dest, distance, road_type, status, time)
        
        # Draw nodes
        for location, (x, y) in self.nodes.items():
            self.draw_node(location, x, y)
    
    def draw_edge(self, source, dest, distance, road_type, status, time):
        """Draw an edge between two nodes"""
        if source not in self.nodes or dest not in self.nodes:
            return
        
        x1, y1 = self.nodes[source]
        x2, y2 = self.nodes[dest]
        
        # Color based on road type
        color_map = {
            'paved': '#27ae60',
            'unpaved': '#f39c12',
            'deep_potholes': '#e74c3c',
            'broken_cisterns': '#9b59b6'
        }
        color = color_map.get(road_type, '#95a5a6')
        
        # Adjust color for status
        if status == 'closed':
            color = '#e74c3c'
        elif status == 'under_repair':
            color = '#f39c12'
        elif status == 'seasonal':
            color = '#e67e22'
        
        # Draw line
        width = 2
        if status == 'closed':
            # Draw dashed line for closed roads
            self.canvas.create_line(x1, y1, x2, y2, 
                                   fill=color, width=width, 
                                   dash=(4, 4),
                                   tags=('edge', f'edge_{source}_{dest}'))
        else:
            self.canvas.create_line(x1, y1, x2, y2, 
                                   fill=color, width=width,
                                   tags=('edge', f'edge_{source}_{dest}'))
        
        # Store edge data
        self.canvas.tag_bind(f'edge_{source}_{dest}', '<Button-1>',
                           lambda e, s=source, d=dest, dist=distance, 
                           rt=road_type, st=status, t=time: 
                           self.show_edge_info(s, d, dist, rt, st, t))
    
    def draw_node(self, location, x, y, highlight=False):
        """Draw a node (location)"""
        radius = 8 if not highlight else 12
        color = '#3498db' if highlight else '#2c3e50'
        
        # Draw circle
        self.canvas.create_oval(x - radius, y - radius, 
                               x + radius, y + radius,
                               fill=color, outline='white', width=2,
                               tags=('node', f'node_{location}'))
        
        # Draw label
        label = location.replace('_', ' ').title()
        self.canvas.create_text(x, y - radius - 10, 
                               text=label,
                               font=('Arial', 8, 'bold'),
                               tags=f'label_{location}')
        
        # Bind click event
        self.canvas.tag_bind(f'node_{location}', '<Button-1>',
                           lambda e, loc=location: self.show_node_info(loc))
    
    def show_node_info(self, location):
        """Display information about a node"""
        self.info_text.delete('1.0', tk.END)
        
        info = f"LOCATION: {location.replace('_', ' ').title()}\n"
        info += "="*35 + "\n\n"
        
        # Get GPS coordinates
        if location in self.coords_data:
            lat, lon = self.coords_data[location]
            info += f"GPS Coordinates:\n"
            info += f"  Latitude:  {lat:.4f}°\n"
            info += f"  Longitude: {lon:.4f}°\n\n"
        
        # Get connected roads
        connected = []
        for source, dest, distance, road_type, status, time in self.edges:
            if source == location:
                connected.append((dest, distance, road_type, status))
            elif dest == location:
                connected.append((source, distance, road_type, status))
        
        if connected:
            info += f"Connected Roads ({len(connected)}):\n"
            for dest, dist, rtype, status in connected:
                info += f"  → {dest.replace('_', ' ').title()}\n"
                info += f"     {dist}km, {rtype}, {status}\n"
        
        self.info_text.insert('1.0', info)
    
    def show_edge_info(self, source, dest, distance, road_type, status, time):
        """Display information about an edge"""
        self.info_text.delete('1.0', tk.END)
        
        info = "ROAD INFORMATION\n"
        info += "="*35 + "\n\n"
        info += f"From: {source.replace('_', ' ').title()}\n"
        info += f"To:   {dest.replace('_', ' ').title()}\n\n"
        info += f"Distance:  {distance} km\n"
        info += f"Type:      {road_type}\n"
        info += f"Status:    {status}\n"
        info += f"Time:      {time} minutes\n\n"
        
        # Calculate average speed
        if time > 0:
            avg_speed = (distance / time) * 60
            info += f"Avg Speed: {avg_speed:.1f} km/h\n"
        
        self.info_text.insert('1.0', info)
    
    def on_canvas_click(self, event):
        """Handle canvas clicks"""
        # This is handled by individual tag bindings
        pass
    
    def highlight_path(self, path):
        """Highlight a specific path on the graph"""
        self.selected_path = path
        
        # Redraw network
        self.draw_network()
        
        # Highlight path edges
        for i in range(len(path) - 1):
            source = path[i]
            dest = path[i + 1]
            
            if source in self.nodes and dest in self.nodes:
                x1, y1 = self.nodes[source]
                x2, y2 = self.nodes[dest]
                
                # Draw highlighted edge
                self.canvas.create_line(x1, y1, x2, y2, 
                                       fill='#e74c3c', width=4,
                                       tags='highlight')
        
        # Highlight path nodes
        for location in path:
            if location in self.nodes:
                x, y = self.nodes[location]
                self.draw_node(location, x, y, highlight=True)


def launch_visualizer():
    """Launch the network visualizer"""
    root = tk.Tk()
    app = NetworkGraphVisualizer(root)
    root.mainloop()


if __name__ == "__main__":
    launch_visualizer()