/**@author <S2084333>
 */
import java.io.File;
import java.io.IOException;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;

public class SaveLoad {
    // =========================================================================
    // ================================= Fields ================================
    // =========================================================================
    private final Model model;
    private final String saveFile;

    // =========================================================================
    // ============================== Constructor ==============================
    // =========================================================================

    public SaveLoad(Model model){
        this.model = model;
        saveFile = "gamesave.txt";
    }

    // =========================================================================
    // ============================= Public Methods ============================
    // =========================================================================

    public int loadGame(){
        model.setBoard(loadGameBoard());
        model.setPlayersTurn(loadPlayerTurn()[0]);
        return loadPlayerTurn()[1];
    }

    public void saveGame() {
        deleteSavedGameFile();
        createFile();
        save();
    }

    // =========================================================================
    // ============================ Private Methods ============================
    // =========================================================================

    private void createFile() {
        try {
            File currentGame = new File(saveFile);
            if (currentGame.createNewFile()) {
                System.out.println("New save file created: " + currentGame.getName());
            }
            else {
                System.out.println("Save file has been updated.");
            }
        } catch (IOException e) {
            System.out.println("An error occurred while trying to create save file. " +
                               "Permission might be needed.");
            e.printStackTrace();
        }
    }

    private void save() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < model.getNrRows(); i++) {
            for (int j = 0; j < model.getNrCols(); j++) {
                builder.append(model.getBoard()[i][j]);
                if (j < model.getNrCols() - 1) {
                    builder.append(" ");
                }
            }
            builder.append("\n");
        }
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(saveFile));
            writer.write(builder.toString());
            writer.write(String.format("%d", model.getPlayersTurn()));
            writer.close();
        } catch (IOException e){
            System.out.println("Game cannot be saved.");
        }
    }

    private void deleteSavedGameFile(){
        try
        {
            Files.deleteIfExists(Paths.get(saveFile));
        }
        catch(NoSuchFileException e)
        {
            System.out.println("No such file/directory exists");
        }
        catch(DirectoryNotEmptyException e)
        {
            System.out.println("Directory is not empty.");
        }
        catch(IOException e)
        {
            System.out.println("Invalid permissions or it's being used by a program.");
        }
        System.out.println("Old save file deletion attempted.");
    }

    private int[] loadPlayerTurn(){
        // Default: {player 1 starts, it's not a NPC game}
        int[] arr = {1, 0};
        try{
            BufferedReader reader = new BufferedReader(new FileReader(saveFile));
            String line = "";
            while (reader.ready()){
                line = reader.readLine();
                if (line.contains("3")){
                    // it is an NPC game
                    arr[1] = 1;
                }
            }
            arr[0] = Integer.parseInt(line);
            System.out.println("Player's turn loaded successfully!");
            return arr;
        } catch (IOException e){
            System.out.println("Saved player's turn order cannot be loaded or the save file doesn't exist, " +
                               "default order is used. You may restart the game to reconfigure.");
        }
        return arr;
    }

    private int[][] loadGameBoard() {
        int[][] board = new int[model.getNrRows()][model.getNrCols()];
        try {
            BufferedReader reader = new BufferedReader(new FileReader(saveFile));
            String line;
            int row = 0;
            while ((line = reader.readLine()).length() > 1) {
                String[] cols = line.split(" ");
                int col = 0;
                for (String element : cols) {
                    board[row][col] = Integer.parseInt(element);
                    col++;
                }
                row++;
            }
            reader.close();
            System.out.println("Game board loaded successfully!");
            return board;
        } catch (IOException e){
            System.out.println("Saved game cannot be loaded or the save file doesn't exist. " +
                               "The default empty board is loaded instead. " +
                               "You may restart the game to reconfigure.");
        }
        return board;
    }
}
