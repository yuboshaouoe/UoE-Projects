/**@author <S2084333>
 */
public class CheckWinner {
    private final Model model;
    private final int[] result;

    // arr[0] = 0 => game not ended
    // arr[0] = 1 => game ended
    // arr[1] = 0 => draw
    // arr[1] = 1 => player 1 wins
    // arr[1] = 2 => player 2 wins
    // arr[1] = 3 => NPC wins
    public void setArr(int a, int b) {
        result[0] = a;
        result[1] = b;
    }

    public CheckWinner(Model model){
        this.model = model;
        result = new int[2];
    }

    public int[] checkWinner(int[][] board){

        // Horizontal

        // for each row
        for (int i = 0; i < model.getNrRows(); i++){

            // for each group of columns (horizontally connected)
            for (int j = 0; j <= model.getNrCols()-model.getConnectN(); j++){

                // counter used to track total connected pieces
                int counterRow = 1;

                for (int n = 1; n < model.getConnectN(); n++){
                    // if all pieces in the group are the same,
                    // and they are not 0s
                    if (board[i][j] == board[i][j+n] && board[i][j] != 0){
                        counterRow++;
                    }
                    // Total count of the connected piece is the same as N
                    // (the given number of connected pieces to win)
                    if (counterRow == model.getConnectN()) {
                        setArr(1, board[i][j]);
                        return result;
                    }
                }
            }
        }

        // Vertical

        // for each group of rows (vertically grouped)
        for (int i = 0; i <= model.getNrRows()-model.getConnectN(); i++){

            // for each column
            for (int j = 0; j < model.getNrCols(); j++){

                int counterCol = 1;

                for (int n = 1; n < model.getConnectN(); n++){

                    if (board[i][j] == board[i+n][j] && board[i][j] != 0){
                        counterCol++;
                    }
                    if (counterCol == model.getConnectN()) {
                        setArr(1, board[i][j]);
                        return result;
                    }
                }
            }
        }

        // Diagonal Left to Right

        // checking from top to bottom
        for (int i = 0; i <= model.getNrRows()-model.getConnectN(); i++){

            // checking from left to right
            for (int j = 0; j <= model.getNrCols()-model.getConnectN(); j++){

                int counterLR = 1;

                for (int n = 1; n < model.getConnectN(); n++){

                    if (board[i][j] == board[i+n][j+n] && board[i][j] != 0) {
                        counterLR++;
                    }
                    if (counterLR == model.getConnectN()) {
                        setArr(1, board[i][j]);
                        return result;
                    }
                }
            }
        }

        // Diagonal Right to Left

        // checking from top to bottom
        for (int i = 0; i <= model.getNrRows()-model.getConnectN(); i++){

            // checking from right to left
            for (int j = model.getNrCols()-1; j >= model.getConnectN()-1; j--){

                int counterRL = 1;

                for (int n = 1; n < model.getConnectN(); n++){

                    if (board[i][j] == board[i+n][j-n] && board[i][j] != 0){
                        counterRL++;
                    }
                    if (counterRL == model.getConnectN()) {
                        setArr(1, board[i][j]);
                        return result;
                    }
                }
            }
        }

        // Full board

        // check if the top row is completely filled
        if (allNotZero(board[0])){
            setArr(1, 0);
            return result;
        }

        return result;
    }

    private boolean allNotZero(int[] row) {
        for (int element : row) {
            if (element == 0) {
                return false;
            }
        }
        return true;
    }
}
