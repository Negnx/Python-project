import com.google.maps.DirectionsApi;
import com.google.maps.DirectionsApiRequest;
import com.google.maps.GeoApiContext;
import com.google.maps.errors.ApiException;
import com.google.maps.model.DirectionsResult;
import com.google.maps.model.DirectionsRoute;
import com.google.maps.model.DirectionsLeg;
import com.google.maps.model.TravelMode;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class Gmaps {
    private static final GeoApiContext context = new GeoApiContext.Builder()
            .apiKey("AIzaSyA6jDrfRWkZIzx1q1O1ZIMW2P-Af4HAv3A") // Hardcode for testing
            .connectTimeout(5, TimeUnit.SECONDS)
            .readTimeout(5, TimeUnit.SECONDS)
            .maxRetries(2)
            .build();

    public static Roads getRoadData(Towns source, Towns destination,
                                    boolean avoidTolls, boolean avoidHighways) {
        try {
            System.out.println("Fetching route: " + source.getName() + " → " + destination.getName());

            DirectionsApiRequest request = DirectionsApi.newRequest(context)
                    .origin(source.getName())
                    .destination(destination.getName())
                    .mode(TravelMode.DRIVING)
                    .alternatives(false);

            if (avoidTolls) request.avoid(DirectionsApi.RouteRestriction.TOLLS);
            if (avoidHighways) request.avoid(DirectionsApi.RouteRestriction.HIGHWAYS);

            DirectionsResult result = request.await();

            if (result.routes == null || result.routes.length == 0) {
                System.err.println("No routes found between " + source.getName() + " and " + destination.getName());
                return null;
            }

            DirectionsRoute route = result.routes[0];
            if (route.legs == null || route.legs.length == 0) {
                System.err.println("Route data incomplete - missing legs");
                return null;
            }

            DirectionsLeg leg = route.legs[0];
            return new Roads(
                    (int)leg.distance.inMeters,
                    (int)leg.duration.inSeconds,
                    route.overviewPolyline != null ? route.overviewPolyline.getEncodedPath() : ""
            );

        } catch (ApiException e) {
            // Handle null error message case
            String errorMsg = e.getMessage() != null ? e.getMessage() : "No error details provided";
            System.err.println("Google Maps API Error: " + errorMsg);

            // Check common error cases safely
            if (errorMsg.contains("REQUEST_DENIED")) {
                System.err.println("→ API key invalid or missing permissions");
            } else if (errorMsg.contains("OVER_QUERY_LIMIT")) {
                System.err.println("→ API quota exceeded");
            } else {
                System.err.println("→ Unknown API error");
            }
        } catch (IOException e) {
            System.err.println("Network error: " + e.getMessage());
        } catch (InterruptedException e) {
            System.err.println("Request interrupted");
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            System.err.println("Unexpected error: " + e.getClass().getSimpleName());
            e.printStackTrace();
        }
        return null;
    }
}