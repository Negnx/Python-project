import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Towns {
	private String name;
	private final Map<Towns, Roads> roads;  // Made final for immutability

	// Constructors
	public Towns() {
		this("");
	}

	public Towns(String name) {
		this.name = Objects.requireNonNull(name, "Town name cannot be null");
		this.roads = new HashMap<>();
	}

	// Copy constructor
	public Towns(Towns other) {
		this(other.name);
		this.roads.putAll(other.roads);
	}

	// Getters and setters with validation
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = Objects.requireNonNull(name, "Town name cannot be null");
	}

	// Returns an unmodifiable view of the roads map
	public Map<Towns, Roads> getRoads() {
		return Collections.unmodifiableMap(roads);
	}

	// Road management methods
	public void addRoad(Towns destination, Roads road) {
		Objects.requireNonNull(destination, "Destination cannot be null");
		Objects.requireNonNull(road, "Road cannot be null");
		if (this.equals(destination)) {
			throw new IllegalArgumentException("Cannot add road to self");
		}
		roads.put(destination, road);
	}

	public void addRoad(Towns destination, int distance, int time) {
		addRoad(destination, new Roads(distance, time, ""));
	}

	public boolean hasRoadTo(Towns destination) {
		return roads.containsKey(destination);
	}

	public Roads getRoadTo(Towns destination) {
		return roads.get(destination);
	}

	public void removeRoad(Towns destination) {
		roads.remove(destination);
	}

	// Overrides
	@Override
	public String toString() {
		return name;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		Towns town = (Towns) o;
		return name.equals(town.name);
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}
}
