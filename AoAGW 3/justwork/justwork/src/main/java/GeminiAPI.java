

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import okhttp3.*;
import java.util.concurrent.TimeUnit;

import java.io.IOException;

public class GeminiAPI {
    private static final String API_KEY = "AIzaSyAgLYS4cOpZMFfXzxo0jkL9jo7Vf_2ceb4"; // 🔹 Replace with your Google Gemini API Key
    private static final String API_URL = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-pro-latest:generateContent?key=" + API_KEY;
    private static final OkHttpClient client = new OkHttpClient.Builder()
            // fixes timeout issue
            .connectTimeout(30, TimeUnit.SECONDS)
            .readTimeout(30, TimeUnit.SECONDS)
            .writeTimeout(30, TimeUnit.SECONDS)
            .build();

    /**
     * Constructs JSON request using Gson.
     * @param prompt The user's prompt for the AI.
     * @return JSON request string.
     */
    private static String buildJsonRequest(String prompt) {
        JsonObject messagePart = new JsonObject();
        messagePart.addProperty("text", prompt);

        JsonArray partsArray = new JsonArray();
        partsArray.add(messagePart);

        JsonObject message = new JsonObject();
        message.addProperty("role", "user");
        message.add("parts", partsArray);

        JsonArray contentsArray = new JsonArray();
        contentsArray.add(message);

        JsonObject requestBody = new JsonObject();
        requestBody.add("contents", contentsArray);

        return requestBody.toString();
    }

    /**
     * Fetches AI-generated route insights from Google Gemini API.
     *
     * @param prompt The user's request.
     * @return AI-generated response with route details.
     */
    public static String getAIResponse(String prompt) {
        String jsonRequest = buildJsonRequest(prompt);

        Request request = new Request.Builder()
                .url(API_URL)
                .post(RequestBody.create(jsonRequest, MediaType.get("application/json")))
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                return "Error: API request failed with code " + response.code() + "\n" + response.body().string();
            }

            // 🔹 Debugging: Print the full API response
            String fullResponse = response.body().string();
            System.out.println("🔍 Full API Response: " + fullResponse);

            return extractResponseText(fullResponse);

        } catch (IOException e) {
            return "Error fetching AI response: " + e.getMessage();
        }
    }

    /**
     * Extracts AI-generated response text from the JSON response.
     */
    private static String extractResponseText(String response) {
        try {
            // 🔹 Debugging: Print response to check format
            System.out.println("🔍 Extracting text from: " + response);

            JsonObject jsonResponse = JsonParser.parseString(response).getAsJsonObject();
            JsonArray candidates = jsonResponse.getAsJsonArray("candidates");

            if (candidates != null && candidates.size() > 0) {
                JsonObject firstCandidate = candidates.get(0).getAsJsonObject();
                JsonObject content = firstCandidate.getAsJsonObject("content");
                JsonArray parts = content.getAsJsonArray("parts");

                if (parts != null && parts.size() > 0) {
                    return parts.get(0).getAsJsonObject().get("text").getAsString();
                }
            }
            return "Error: No valid AI-generated response found.";
        } catch (Exception e) {
            return "Error parsing API response: " + e.getMessage();
        }
    }

   /*public static void main(String[] args) {
        String prompt = "Find the shortest route from Kingston to Montego Bay avoiding toll roads.";
        String aiResponse = getAIResponse(prompt);

        System.out.println("📌 AI Response:");
        System.out.println(aiResponse);
    }*/
}

