


import java.util.List;

public class Path 
{
	private List<Towns> towns; 
    private double ttlDistance; 
    private int ttlTime; 

    public Path(List<Towns> towns, double ttlDistance, int ttlTime) 
    {
        this.towns = towns;
        this.ttlDistance = ttlDistance;
        this.ttlTime = ttlTime;
    }

    @Override
    public String toString() 
    {
        return "Shortest Path: " + towns + "\nTotal Distance: " + ttlDistance + " km" + "\nTotal Time: " + ttlTime + " minutes";
    }
}
