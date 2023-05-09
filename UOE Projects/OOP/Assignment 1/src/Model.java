/**
 * This file is to be completed by you.
 *
 * @author <S2084333>
 */
public final class Model
{
	// ===========================================================================
	// ================================ CONSTANTS ================================
	// ===========================================================================
	// The most common version of Connect Four has 7 rows and 6 columns.
	public static final int DEFAULT_NR_ROWS = 7;
	public static final int DEFAULT_NR_COLS = 6;
	public static final int DEFAULT_N = 4;
	
	// ========================================================================
	// ================================ FIELDS ================================
	// ========================================================================
	// The size of the board
	private int nrRows;
	private int nrCols;

	// 2D array representing the board.
	private int[][] board;

	private int playersTurn;

	// The required number of connected pieces to win.
	private int connectN;
	
	// =============================================================================
	// ================================ CONSTRUCTOR ================================
	// =============================================================================
	public Model() {

		// Initialise the board size and N (the required number of connected pieces to win) to given values.
		nrRows = DEFAULT_NR_ROWS;
		nrCols = DEFAULT_NR_COLS;
		connectN = DEFAULT_N;

		// It is assumed that player 1 goes first in the first game.
		// if a new session is started, the losing player goes first.
		playersTurn = 1;

		board = new int[getNrRows()][getNrCols()];
	}
	
	// ====================================================================================
	// ================================ MODEL INTERACTIONS ================================
	// ====================================================================================

	public boolean isMoveValid(int move) {
		// if input is out of range of the given columns OR a column is full
		if (move == 0 || move > getNrCols() || getBoard()[0][move - 1] != 0){
			return false;
		}
		else
			return true;
	}

	public void makeMove(int move) {
		if (isPlayer1sTurn()){
			playMove(move, 1, getBoard());
		}
		else
			playMove(move, 2, getBoard());
	}

	// =========================================================================
	// ============================ Private Methods ============================
	// =========================================================================

	private boolean isPlayer1sTurn(){
		return getPlayersTurn() == 1;
	}

	private void playMove(int move, int player, int[][] board){
		for (int i = getNrRows()-1; i >= 0; i--){
			if (board[i][move-1] == 0){
				board[i][move-1] = player;
				break;
			}
		}
	}

	// =========================================================================
	// =========================== GETTERS & SETTERS ===========================
	// =========================================================================
	public int getNrRows() {
		return nrRows;
	}

	public void setNrRows(int rows){
		nrRows = rows;
	}
	
	public int getNrCols() {
		return nrCols;
	}

	public void setNrCols(int cols){
		nrCols = cols;
	}

	public int getPlayersTurn(){
		return playersTurn;
	}

	public void setPlayersTurn(int n){
		playersTurn = n;
	}

	public int[][] getBoard(){
		return board;
	}

	public void setBoard(int rows, int cols){
		board = new int[rows][cols];
	}

	public void setBoard(int[][] board) {
		this.board = board;
	}

	public int getConnectN(){
		return connectN;
	}

	public void setConnectN(int n){
		connectN = n;
	}
}
