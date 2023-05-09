/**
 * This file is to be completed by you.
 *
 * @author <S2084333>
 */
public final class Controller
{
	// =========================================================================
	// ================================= Fields ================================
	// =========================================================================
	private final Model model;
	private final TextView view;
	private final CheckWinner check;
	private final SaveLoad saveLoad;
	private final NPC npc;

	// =========================================================================
	// ============================== Constructor ==============================
	// =========================================================================
	
	public Controller(Model model, TextView view, CheckWinner check, SaveLoad saveLoad, NPC npc) {
		this.model = model;
		this.view = view;
		this.check = check;
		this.saveLoad = saveLoad;
		this.npc = npc;
	}

	// =========================================================================
	// ============================= START SESSION! ============================
	// =========================================================================
	public void startSession() {
		if (view.promptForLoad() == 'Y'){
			if (saveLoad.loadGame() == 0){
				playerIteration();
			}
			else
			NPCIteration();
		}
		else {
			NPCOrPlayers();
		}
	}

	// =========================================================================
	// =============================== Game Flow ===============================
	// =========================================================================

	private void NPCOrPlayers(){
		if (view.promptForNPCGame() == 'Y'){
			System.out.println("Configure your new game!");
			gameReset();
			view.displayNewGameMessage();
			NPCIteration();
		}
		else {
			System.out.println("Configure your new game!");
			gameReset();
			view.displayNewGameMessage();
			playerIteration();
		}
	}

	private void newSession(){
		if (view.promptForNewGame() == 'Y'){
			startSession();
		}
		else
			System.out.println("Thank you for playing!");
	}

	private void gameReset(){
		model.setNrRows(view.promptForRows());
		model.setNrCols(view.promptForColumns());
		model.setConnectN(view.promptForN());
		model.setBoard(model.getNrRows(), model.getNrCols());
		model.setPlayersTurn(1);
		check.setArr(0, 0);
	}

	// =========================================================================
	// ============================= Player's Turn =============================
	// =========================================================================

	private void playerIteration(){
		view.displayBoard();
		// while the game hasn't ended
		while (check.checkWinner(model.getBoard())[0] == 0)
			playerTurn();
		view.gameReport();
		newSession();
	}

	private void playerTurn(){
		view.displayTurn();
		model.makeMove(view.askForMoveAndValidify());
		view.displayBoard();
		if (model.getPlayersTurn() == 1){
			model.setPlayersTurn(2);
		}
		else
			model.setPlayersTurn(1);
	}

	// =========================================================================
	// ============================== NPC's turn ===============================
	// =========================================================================
	private void NPCIteration(){
		view.displayBoard();
		while (check.checkWinner(model.getBoard())[0] == 0){
			view.displayTurn();
			model.makeMove(view.askForMoveAndValidify());
			view.displayBoard();
			if (check.checkWinner(model.getBoard())[0] == 0) {
				npc.NPCMakeMove();
				System.out.println("NPC has made a move.");
				view.displayBoard();
			}
		}
		view.gameReport();
		newSession();
	}
}
