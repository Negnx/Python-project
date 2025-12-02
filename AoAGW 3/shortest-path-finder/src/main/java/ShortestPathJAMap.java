import java.util.*;

class ShortestPathJAMap {
    // each edge has a destination(connected Node), weight (travel time in hours), and distance (in km)
    static class Edge {
        int destination;
        double weight; // travel time in hours
        double distance; // distance in km

        Edge(int destination, double weight, double distance) {
            this.destination = destination;
            this.weight = weight;
            this.distance = distance;
        }
    }

    // creates an adjacency list (to keep track of previous nodes, distance, and travel times)
    static class Graph {
        int cities;
        List<List<Edge>> adjacencyList;
        String[] cityNames;

        Graph(int cities, String[] cityNames) {
            this.cities = cities;
            this.cityNames = cityNames;
            adjacencyList = new ArrayList<>(cities);
            for (int i = 0; i < cities; i++) {
                adjacencyList.add(new ArrayList<>());
            }
        }

        // adds an edge to the adjacency list (including distance and travel time)
        void addEdge(int source, int destination, double weight, double distance) {
            adjacencyList.get(source).add(new Edge(destination, weight, distance));
            adjacencyList.get(destination).add(new Edge(source, weight, distance));
        }

        // Dijkstra's algorithm for shortest time and distance
        String dijkstra(int source, int destination) {
            double[] time = new double[cities];
            double[] dist = new double[cities];
            Arrays.fill(time, Double.MAX_VALUE);
            Arrays.fill(dist, Double.MAX_VALUE);
            time[source] = 0;
            dist[source] = 0;

            PriorityQueue<Edge> pq = new PriorityQueue<>(Comparator.comparingDouble(e -> e.weight));
            pq.offer(new Edge(source, 0, 0));

            int[] previous = new int[cities];
            Arrays.fill(previous, -1);

            while (!pq.isEmpty()) {
                Edge current = pq.poll();
                int currentCity = current.destination;

                if (currentCity == destination)
                    break;

                for (Edge neighbor : adjacencyList.get(currentCity)) {
                    double newTime = time[currentCity] + neighbor.weight;
                    double newDist = dist[currentCity] + neighbor.distance;

                    if (newTime < time[neighbor.destination]) {
                        time[neighbor.destination] = newTime;
                        dist[neighbor.destination] = newDist;
                        previous[neighbor.destination] = currentCity;
                        pq.offer(new Edge(neighbor.destination, newTime, newDist));
                    }
                }
            }

            // Build output string
            StringBuilder result = new StringBuilder();
            result.append("Shortest time from ").append(cityNames[source]).append(" to ").append(cityNames[destination]).append(":\n");
            result.append("Total time: ").append(String.format("%.2f", time[destination])).append(" hours\n");
            result.append("Total distance: ").append(String.format("%.2f", dist[destination])).append(" km\n");

            // Reconstruct path
            List<Integer> path = new ArrayList<>();
            for (int at = destination; at != -1; at = previous[at]) {
                path.add(at);
            }
            Collections.reverse(path);
            result.append("Route:\n");
            for (int i = 0; i < path.size(); i++) {
                result.append(cityNames[path.get(i)]);
                if (i < path.size() - 1) {
                    result.append(" -> ");
                }
            }

            return result.toString();
        }

    }


}
