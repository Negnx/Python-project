import java.awt.Desktop;
import java.net.URI;

public class MapOpener {
    public static void openRoute(String source, String destination) {
        try {
            String url = String.format(
                    "https://www.google.com/maps/dir/%s,Jamaica/%s,Jamaica",
                    source.replace(" ", "+"),
                    destination.replace(" ", "+"));

            if (Desktop.isDesktopSupported()) {
                Desktop.getDesktop().browse(new URI(url));
            } else {
                System.err.println("Desktop operations not supported");
            }
        } catch (Exception e) {
            System.err.println("Error opening maps: " + e.getMessage());
        }
    }
}