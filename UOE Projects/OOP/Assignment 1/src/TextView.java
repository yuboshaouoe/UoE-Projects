/**
 * This file is to be completed by you.
 *
 * @author <S2084333>
 */
public final class TextView {
	// =========================================================================
	// ==============================  Fields  =================================
	// =========================================================================
	private final Model model;
	private final CheckWinner check;
	private final SaveLoad saveLoad;
	private final String rowDividerUnit = "-".repeat(4);

	// =========================================================================
	// =============================  Constructor  =============================
	// =========================================================================

	public TextView(Model model, CheckWinner check, SaveLoad saveLoad) {
		this.model = model;
		this.check = check;
		this.saveLoad = saveLoad;
	}

	// =========================================================================
	// ===========================  General Methods  ===========================
	// =========================================================================
	public final void displayNewGameMessage() {
		System.out.println("---- NEW GAME STARTED ----");
	}
	
	public final int MoveOrSave() {
		String prompt = "Type 'save' if you would like to save the current game.\nOtherwise, enter a free column: ";
		System.out.print(prompt);
		String userInput = InputUtil.readStringFromUser().toLowerCase();
		// isParsable determines whether the given string can be parsed as an integer.
		while (!userInput.equals("save") && !isParsable(userInput)){
			System.out.println("Please enter 'save' or an integer!");
			System.out.print(prompt);
			userInput = InputUtil.readStringFromUser().toLowerCase();
		}
		if (userInput.equals("save")) {
			saveLoad.saveGame();
			System.out.print("Now enter a free column: ");
			userInput = InputUtil.readStringFromUser();
			while (!isParsable(userInput)) {
				System.out.println("Please enter an integer!");
				System.out.print(prompt);
				userInput = InputUtil.readStringFromUser();
			}
		}
		return Integer.parseInt(userInput);
	}
	
	public final int askForMoveAndValidify(){
		int move = MoveOrSave();
		while (!model.isMoveValid(move)) {
			System.out.println("It is not a legal move!");
			move = MoveOrSave();
		}
		return move;
	}

	public void displayTurn(){
		if (model.getPlayersTurn() == 1){
			System.out.println("It is player 1's turn.");
		}
		else
			System.out.println("It is player 2's turn.");
	}

	// from the CheckWinner array, report the game result.
	public void gameReport(){
		if (check.checkWinner(model.getBoard())[1] == 1){
			System.out.println("Player 1 wins!");
		}
		else if (check.checkWinner(model.getBoard())[1] == 2){
			System.out.println("Player 2 wins!");
		}
		else if (check.checkWinner(model.getBoard())[1] == 3){
			System.out.println("NPC wins!");
		}
		else
			System.out.println("The Game is a draw.");
	}

	// =========================================================================
	// ============================  Display Board  ============================
	// =========================================================================
	public final void displayBoard()
	{
		// A StringBuilder is used to assemble longer Strings more efficiently.
		StringBuilder sb = new StringBuilder();
		
		// Board construction.

		// Constructed board is based on the given 2D array.
		// When the 2D array is updated, a new board will be constructed based on the new array.

		// Dividers are spilt into units. Depending on the given number of rows and cols,
		// different rowDivider and columnDivider are constructed.

		for (int i = 0; i < model.getNrRows(); i++) {

			// append 1 rowDivider
			sb.append("-").append(rowDividerUnit.repeat(model.getNrCols()));
			sb.append("\n");

			// Append the column units depending on the elements of the board array:
			// 1 = player 1, 2 = player 2, 0 = empty.
			for (int n = 0; n < model.getNrCols(); n++){
				String columnDividerUnit = "|   ";
				if (model.getBoard()[i][n] == 1) {
					String filledPlayer1Unit = "| O ";
					sb.append(filledPlayer1Unit);
				}
				else if (model.getBoard()[i][n] == 2){
					String filledPlayer2Unit = "| X ";
					sb.append(filledPlayer2Unit);
				}
				else if (model.getBoard()[i][n] == 3){
					sb.append("| @ ");
				}
				else
					sb.append(columnDividerUnit);
			}
			sb.append("|\n");
		}

		// Add 1 RowDivider to close the bottom of the board.
		sb.append("-").append(rowDividerUnit.repeat(model.getNrCols()));

		System.out.println(sb);
	}

	private boolean isParsable(String input){
		try {
			Integer.parseInt(input);
			return true;
		} catch (final NumberFormatException e) {
			return false;
		}
	}

	// =========================================================================
	// ============================== Prompt User ==============================
	// =========================================================================

	public char promptForNPCGame(){
		char[] YorN = {'Y', 'N'};
		return promptForValue("Would you like to play against a NPC?\nIf no, you will need another player. (Y/N): ",
				YorN, "Please enter 'Y' or 'N'! (Just the character)");
	}

	public char promptForNewGame(){
		char[] YorN = {'Y', 'N'};
		return promptForValue("Start a new game? (Y/N): ",
				YorN, "Please enter 'Y' or 'N'! (Just the character)");
	}

	public char promptForLoad(){
		char[] YorN = {'Y', 'N'};
		return promptForValue("Would you like to load from the previous save? (Y/N): ",
				YorN, "Please enter 'Y' or 'N'! (Just the character)");
	}

	public int promptForRows(){
		return promptForValue("Number of rows of the board (rows >= 3): ",
				3, "Number of rows must be greater than 3!");
	}

	public int promptForColumns(){
		return promptForValue("Number of columns of the board (columns >= 3): ",
				3, "Number of columns must be greater than 3!");
	}

	public int promptForN(){
		System.out.print("Enter how many connected pieces needed to win (number of columns >= n >= 3): ");
		int value = InputUtil.readIntFromUser();
		while (value < 3 || value > model.getNrCols()){
			System.out.println("n cannot be less than 3 or more than the number of columns.");
			System.out.print("Enter how many connected pieces needed to win (number of columns >= n >= 3): ");
			value = InputUtil.readIntFromUser();
		}
		return value;
	}

	private int promptForValue(String prompt, int valueRangeMax, String errorMessage){
		System.out.print(prompt);
		int value = InputUtil.readIntFromUser();
		while (value < valueRangeMax){
			System.out.println(errorMessage);
			System.out.print(prompt);
			value = InputUtil.readIntFromUser();
		}
		return value;
	}

	private boolean isContained(char input, char[]arr){
		boolean found = false;
		for (char c : arr){
			if (c == input){
				found = true;
				break;
			}
		}
		return found;
	}

	private char promptForValue(String prompt, char[] chars, String errorMessage){
		System.out.print(prompt);
		char value = InputUtil.readCharFromUser();
		// check if user input is in the array of accepted inputs
		while (!isContained(value, chars)){
			System.out.println(errorMessage);
			System.out.print(prompt);
			value = InputUtil.readCharFromUser();
		}
		return value;
	}
}