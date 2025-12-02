import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.stage.Stage;
public class Main extends Application {

    private final String[] cityNames = {
            "Ocho Rios", "St Anns", "Falmouth", "Montego Bay", "Lucea", "Negril",
            "Savana-Lamar", "Black River", "Santa Cruz", "Alley", "May Pen", "Spanish Town",
            "Kingston", "Port Royal", "Annotto Bay", "Port Maria", "Morant Point", "Port Antonio",
            "Ewarton", "Christiana", "Mandeville"
    };

    private ComboBox<String> sourceComboBox;
    private ComboBox<String> destinationComboBox;
    private TextArea resultArea;
    private CheckBox avoidTolls;
    private CheckBox avoidHighways;
    private CheckBox avoidHills;
    private Button openMapButton;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("Jamaica Shortest Path Finder");

        // Create main container with padding
        VBox mainContainer = new VBox(15);
        mainContainer.setPadding(new Insets(20));
        mainContainer.setStyle("-fx-background-color: #f5f5f5;");

        // Create header
        Label headerLabel = new Label("Jamaica Route Planner");
        headerLabel.setFont(Font.font("Arial", FontWeight.BOLD, 24));
        headerLabel.setTextFill(Color.web("#2c3e50"));
        VBox.setMargin(headerLabel, new Insets(0, 0, 20, 0));

        // Create input panel
        GridPane inputPanel = new GridPane();
        inputPanel.setHgap(15);
        inputPanel.setVgap(15);
        inputPanel.setPadding(new Insets(15));
        inputPanel.setStyle("-fx-background-color: white; -fx-border-radius: 5; -fx-background-radius: 5;");

        // Create and style combo boxes
        sourceComboBox = new ComboBox<>();
        sourceComboBox.getItems().addAll(cityNames);
        sourceComboBox.setPromptText("Select Source City");
        sourceComboBox.setPrefWidth(250);

        destinationComboBox = new ComboBox<>();
        destinationComboBox.getItems().addAll(cityNames);
        destinationComboBox.setPromptText("Select Destination City");
        destinationComboBox.setPrefWidth(250);

        inputPanel.add(new Label("Source:"), 0, 0);
        inputPanel.add(sourceComboBox, 1, 0);
        inputPanel.add(new Label("Destination:"), 0, 1);
        inputPanel.add(destinationComboBox, 1, 1);

        // Create options panel
        HBox optionsPanel = new HBox(20);
        optionsPanel.setPadding(new Insets(10));
        optionsPanel.setAlignment(Pos.CENTER);
        optionsPanel.setStyle("-fx-background-color: white; -fx-border-radius: 5; -fx-background-radius: 5;");

        avoidTolls = new CheckBox("Avoid Tolls");
        avoidHighways = new CheckBox("Avoid Highways");
        avoidHills = new CheckBox("Avoid Hills");

        // Style checkboxes
        String checkboxStyle = "-fx-font-size: 14; -fx-text-fill: #34495e;";
        avoidTolls.setStyle(checkboxStyle);
        avoidHighways.setStyle(checkboxStyle);
        avoidHills.setStyle(checkboxStyle);

        optionsPanel.getChildren().addAll(avoidTolls, avoidHighways, avoidHills);

        // Create button panel
        HBox buttonPanel = new HBox(15);
        buttonPanel.setAlignment(Pos.CENTER);
        buttonPanel.setPadding(new Insets(10));

        Button calculateButton = new Button("Calculate");
        openMapButton = new Button("Open in Google Maps");

        // Style buttons
        calculateButton.setStyle("-fx-background-color: #4CAF50; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 8 15;");
        openMapButton.setStyle("-fx-background-color: #2196F3; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 8 15;");

        // Add hover effects
        calculateButton.setOnMouseEntered(e -> calculateButton.setStyle("-fx-background-color: #3e8e41; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 8 15;"));
        calculateButton.setOnMouseExited(e -> calculateButton.setStyle("-fx-background-color: #4CAF50; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 8 15;"));

        openMapButton.setOnMouseEntered(e -> openMapButton.setStyle("-fx-background-color: #0b7dda; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 8 15;"));
        openMapButton.setOnMouseExited(e -> openMapButton.setStyle("-fx-background-color: #2196F3; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 8 15;"));

        buttonPanel.getChildren().addAll(calculateButton, openMapButton);

        // Result area
        resultArea = new TextArea();
        resultArea.setEditable(false);
        resultArea.setWrapText(true);
        resultArea.setPrefHeight(250);
        resultArea.setStyle("-fx-font-family: monospace;");

        // Add all components to main container
        mainContainer.getChildren().addAll(
                headerLabel,
                inputPanel,
                optionsPanel,
                buttonPanel,
                resultArea
        );

        // Set up event handlers
        calculateButton.setOnAction(e -> calculatePath());
        openMapButton.setOnAction(e -> openInBrowser());
        avoidTolls.setOnAction(e -> handleCheckboxChange());
        avoidHighways.setOnAction(e -> handleCheckboxChange());
        avoidHills.setOnAction(e -> handleCheckboxChange());

        // Set scene
        Scene scene = new Scene(mainContainer, 600, 600);
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    private void handleCheckboxChange() {
        System.out.println("Tolls: " + avoidTolls.isSelected() +
                ", Highways: " + avoidHighways.isSelected() +
                ", Hills: " + avoidHills.isSelected());
    }

    private void calculatePath() {
        String source = sourceComboBox.getValue();
        String destination = destinationComboBox.getValue();

        if (source == null || destination == null) {
            resultArea.setText("Please select both source and destination cities.");
            return;
        }

        boolean avoidTollsSelected = avoidTolls.isSelected();
        boolean avoidHighwaysSelected = avoidHighways.isSelected();
        boolean avoidHillsSelected = avoidHills.isSelected();

        // Find the indices of the selected cities
        int sourceIndex = -1, destinationIndex = -1;
        for (int i = 0; i < cityNames.length; i++) {
            if (cityNames[i].equals(source)) {
                sourceIndex = i;
            }
            if (cityNames[i].equals(destination)) {
                destinationIndex = i;
            }
        }

        if (sourceIndex == -1 || destinationIndex == -1) {
            resultArea.setText("Invalid city selection. Please try again.");
            return;
        }

        // Initialize the graph
        ShortestPathJAMap.Graph graph = new ShortestPathJAMap.Graph(cityNames.length, cityNames);

        // Adding edges with travel times in hours (converted from minutes) and distances in km
        graph.addEdge(12, 16, 105.0 / 60, 87.4);  // Kingston - Morant Point
        graph.addEdge(12, 17, 124.0 / 60, 92.2);  // Kingston - Port Antonio
        graph.addEdge(12, 14, 73.0 / 60, 46.82);  // Kingston - Annotto Bay
        graph.addEdge(12, 11, 28.0 / 60, 20.4);   // Kingston - Spanish Town
        graph.addEdge(12, 13, 32.0 / 60, 26.3);   // Kingston - Port Royal
        graph.addEdge(16, 17, 83.0 / 60, 67.7);   // Morant Point - Port Antonio
        graph.addEdge(17, 14, 52.0 / 60, 45.5);   // Port Antonio - Annotto Bay
        graph.addEdge(14, 15, 27.0 / 60, 25.2);   // Annotto Bay - Port Maria
        graph.addEdge(15, 0, 32.0 / 60, 31.4);    // Port Maria - Ocho Rios
        graph.addEdge(0, 1, 14.0 / 60, 12.1);    // Ocho Rios - St Anns Bay
        graph.addEdge(1, 2, 57.0 / 60, 57.1);    // St Anns Bay - Falmouth
        graph.addEdge(2, 3, 36.0 / 60, 34.9);    // Falmouth - Montego Bay
        graph.addEdge(3, 4, 37.0 / 60, 36.5);    // Montego Bay - Lucea
        graph.addEdge(4, 5, 37.0 / 60, 39.6);    // Lucea - Negril
        graph.addEdge(11, 9, 68.0 / 60, 54.7);   // Spanish Town - Alley
        graph.addEdge(9, 8, 106.0 / 60, 93.1);   // Alley - Santa Cruz
        graph.addEdge(8, 7, 28.0 / 60, 28.9);    // Santa Cruz - Black River
        graph.addEdge(7, 6, 47.0 / 60, 47.3);    // Black River - Savana-Lamar
        graph.addEdge(6, 5, 29.0 / 60, 28.1);    // Savana-Lamar - Negril
        graph.addEdge(11, 18, 43.0 / 60, 30.5);  // Spanish Town - Ewarton
        graph.addEdge(18, 0, 48.0 / 60, 46.8);   // Ewarton - Ocho Rios
        graph.addEdge(18, 19, 117.0 / 60, 77.6); // Ewarton - Christiana
        graph.addEdge(11, 10, 37.0 / 60, 34.6);  // Spanish Town - May Pen
        graph.addEdge(10, 9, 45.0 / 60, 24.5);   // May Pen - Alley
        graph.addEdge(10, 20, 44.0 / 60, 39.4);  // May Pen - Mandeville
        graph.addEdge(20, 19, 32.0 / 60, 20.9);  // Mandeville - Christiana
        graph.addEdge(19, 2, 93.0 / 60, 70.2);   // Christiana - Falmouth
        graph.addEdge(19, 8, 54.0 / 60, 51.2);   // Christiana - Santa Cruz
        graph.addEdge(6, 3, 59.0 / 60, 49.5);    // Savana-Lamar - Montego Bay

        // Calculate the shortest path
        StringBuilder result = new StringBuilder();
        String pathResult = graph.dijkstra(sourceIndex, destinationIndex);
        result.append(pathResult);

        // Add AI-generated insights (if implemented)
        try {
            String aiPrompt = String.format(
                    "Provide travel insights from %s to %s in Jamaica %s %s %s",
                    source, destination,
                    avoidTollsSelected ? "avoiding tolls" : "",
                    avoidHighwaysSelected ? "avoiding highways" : "",
                    avoidHillsSelected ? "avoiding hilly areas" : "");


                String aiResponse = GeminiAPI.getAIResponse(aiPrompt);
             result.append("\n\n=== TRAVEL TIPS ===\n").append(aiResponse);
        } catch (Exception e) {
            result.append("\nCouldn't load travel tips: ").append(e.getMessage());
        }

        resultArea.setText(result.toString());
    }

    private void openInBrowser() {
        String source = sourceComboBox.getValue();
        String destination = destinationComboBox.getValue();

        if (source != null && destination != null) {
            MapOpener.openRoute(source, destination);
            resultArea.appendText("\n\nGoogle Maps integration would open here for " +
                    source + " to " + destination);
        }
    }
}