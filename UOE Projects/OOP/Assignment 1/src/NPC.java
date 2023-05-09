/**@author <S2084333>
 */
import java.util.Random;


public class NPC {
    private final Model model;
    private final CheckWinner check;

    public NPC(Model model, CheckWinner check){
        this.model = model;
        this.check = check;
    }

    public void NPCMakeMove() {
        /*
        if (checkWinningMove() != 0) {
            for (int i = model.getNrRows() - 1; i >= 0; i--) {
                if (model.getBoard()[i][checkWinningMove() - 1] == 0) {
                    model.getBoard()[i][checkWinningMove() - 1] = 3;
                    break;
                }
            }
        }
        if (preventWinningMove() != 0) {
            for (int i = model.getNrRows() - 1; i >= 0; i--) {
                if (model.getBoard()[i][preventWinningMove() - 1] == 0) {
                    model.getBoard()[i][preventWinningMove() - 1] = 3;
                    break;
                }
            }
        }
        else {*/
            int posMove = randInt(1, model.getNrCols());
            while (!model.isMoveValid(posMove)) {
                posMove = randInt(1, model.getNrCols());
            }
            for (int i = model.getNrRows() - 1; i >= 0; i--) {
                if (model.getBoard()[i][posMove - 1] == 0) {
                    model.getBoard()[i][posMove - 1] = 3;
                    break;

                }
            }
    }

    private int randInt(int min, int max){
        Random rand = new Random();
        return rand.nextInt((max - min) + 1) + min;
    }

    private int checkWinningMove() {
        // Initialize virtual board as a copy of the actual board
        int[][] virtualBoard = model.getBoard();
        // for every column
        for (int i = 1; i <= model.getNrCols(); i++) {
            if (model.isMoveValid(i)) {
                // Play the move
                for (int n = model.getNrRows() - 1; n >= 0; n--) {
                    if (virtualBoard[n][i-1] == 0) {
                        virtualBoard[n][i-1] = 3;
                        break;
                    }
                }
                // if by playing this move NPC doesn't win
                if (check.checkWinner(virtualBoard)[0] != 1){
                    // reset the virtual board
                    virtualBoard = model.getBoard();
                }
                // if wins
                else
                    return i;
            }
        }
        // 0 => random move
        return 0;
    }

    private int preventWinningMove(){

        int[][] virtualBoard = model.getBoard();

        for (int i = 1; i <= model.getNrCols(); i++) {

            if (model.isMoveValid(i)) {

                for (int n = model.getNrRows() - 1; n >= 0; n--) {
                    if (virtualBoard[n][i-1] == 0) {
                        virtualBoard[n][i-1] = 1;
                        break;
                    }
                }
                // If by playing this move the player doesn't win
                if (check.checkWinner(virtualBoard)[0] != 1){
                    virtualBoard = model.getBoard();
                }
                else
                    return i;
            }
        }
        return 0;
    }
}
