public class Roads {
	private int distance;  // in meters
	private int time;      // in seconds
	private String polyline;

	// Default constructor
	public Roads() {
		this(0, 0, "");
	}

	// Parameterized constructor
	public Roads(int distance, int time, String polyline) {
		setDistance(distance);
		setTime(time);
		setPolyline(polyline);
	}

	// Copy constructor
	public Roads(Roads rd) {
		this(rd.distance, rd.time, rd.polyline);
	}

	// Getters and setters with validation
	public int getDistance() {
		return distance;
	}

	public void setDistance(int distance) {
		if (distance < 0) {
			throw new IllegalArgumentException("Distance cannot be negative");
		}
		this.distance = distance;
	}

	public int getTime() {
		return time;
	}

	public void setTime(int time) {
		if (time < 0) {
			throw new IllegalArgumentException("Time cannot be negative");
		}
		this.time = time;
	}

	public String getPolyline() {
		return polyline;
	}

	public void setPolyline(String polyline) {
		this.polyline = polyline != null ? polyline : "";
	}

	// Helper methods for converted units
	public double getDistanceInKm() {
		return distance / 1000.0;
	}

	public double getTimeInMinutes() {
		return time / 60.0;
	}

	public double getTimeInHours() {
		return time / 3600.0;
	}

	@Override
	public String toString() {
		return String.format("Distance: %.2f km\nTime: %.1f minutes\nPolyline: %s",
				getDistanceInKm(),
				getTimeInMinutes(),
				polyline.isEmpty() ? "Not available" : "[encoded]");
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) return true;
		if (obj == null || getClass() != obj.getClass()) return false;
		Roads other = (Roads) obj;
		return distance == other.distance &&
				time == other.time &&
				polyline.equals(other.polyline);
	}

	@Override
	public int hashCode() {
		int result = distance;
		result = 31 * result + time;
		result = 31 * result + polyline.hashCode();
		return result;
	}
}
