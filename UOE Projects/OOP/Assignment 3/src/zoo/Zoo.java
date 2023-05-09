package zoo;

import animals.Animal;
import areas.*;
import dataStructures.CashCount;
import dataStructures.ICashCount;
import java.util.ArrayList;
import java.util.HashMap;

public class Zoo implements IZoo{

    // The counter is used to give an unique ID for areas in the zoo.
    private int counter = 0;

    private final HashMap<Integer, HashMap<IArea, ArrayList<Animal>>> areas = new HashMap<>();
    private final ArrayList<Integer> reachableAreas = new ArrayList<>();

    private final HashMap<String, Integer> entranceFee = new HashMap<>();
    private ICashCount cashCount = new CashCount();

    public Zoo(){
        // Add an initial entrance to the zoo.
        Entrance initEntrance = new Entrance();
        // Initial entrance would have ID 0.
        addArea(initEntrance);
        // Set the format of entranceFee Hashmap.
        entranceFee.put("Pounds", 0);
        entranceFee.put("Pence", 0);
    }

    /**
     * Pair the new area with an Arraylist for the animals inside.
     * (NonHabitat would have no animals)
     * @param area The area to be paired with.
     * @return A HashMap where key is input area and value is the Arraylist
     */
    private HashMap<IArea, ArrayList<Animal>> areaInitializer(IArea area){
        var areaHashMap = new HashMap<IArea, ArrayList<Animal>>();
        var animalArraylist = new ArrayList<Animal>();
        areaHashMap.put(area, animalArraylist);
        return areaHashMap;
    }

    /**
     * Adds a new area to the zoo.
     * @param area The area to be added.
     * @return An ID by which the added area can be uniquely identified.
     */
    public int addArea(IArea area){
        for (int areaId : areas.keySet()){
            if (getArea(areaId).equals(area)){
                System.out.println("The input area has been added before.");
                return -1;
            }
        }
        int id = counter;
        areas.put(id, areaInitializer(area));
        counter++;
        return id;
        }

    /**
     * Removes the specified area from the zoo.
     * @param areaId The ID of the area to be removed.
     * @exception IndexOutOfBoundsException if the input id doesn't exist.
     */
    public void removeArea(int areaId){
        try {
            // The initial entrance cannot be removed
            if (areaId != 0){
                // remove the connections with this area in other areas
                for (int id : areas.keySet()){
                    getArea(id).removeAdjacentArea(areaId);
                }
                areas.remove(areaId);
            }
            else {
                System.out.println("The initial Entrance cannot be removed.");
            }
        } catch (IndexOutOfBoundsException e){
            System.out.println("The area to be removed doesn't exist.");
        }
    }

    /**
     * Returns the area associated with the given ID.
     * @param areaId The ID of the area to be fetched.
     * @return The area corresponding to the given ID.
     */
    public IArea getArea(int areaId){
            return areas.get(areaId).keySet().iterator().next();
    }

    /**
     * Tries to add the given animal to the specified area.
     * @param areaId The ID of the area the animal is to be added to.
     * @param animal The animal to be added.
     * @return Returns a code indicating success or failure. See {@link Codes}.
     */
    public byte addAnimal(int areaId, Animal animal){
        if (!areas.containsKey(areaId)){
            System.out.println("The area doesn't exist.");
            return -1;
        }
        var area = getArea(areaId);
        var name = area.getClass().getSimpleName();
        // if area is not a habitat
        if (area instanceof NonHabitat){
            return Codes.NOT_A_HABITAT;
        }
        // if the animal doesn't fit the habitat area
        if (!name.equals(animal.getHabitat())){
            return Codes.WRONG_HABITAT;
        }
        if (area instanceof Habitat){
            Habitat habitat = (Habitat)area;
            var animalList = areas.get(areaId).get(area);
            // if the habitat is full
            if (animalList.size() >= habitat.getMax()){
                return Codes.HABITAT_FULL;
            }
            // if an animal is not compatible with another animal in the habitat
            for (Animal a : animalList){
                if (!animal.isCompatibleWith(a)){
                    return Codes.INCOMPATIBLE_INHABITANTS;
                }
            }
        }
        areas.get(areaId).get(area).add(animal);
        return Codes.ANIMAL_ADDED;
    }

    /**
     * Allows visitors to move between areas in the given direction (from -> to).
     * @param fromAreaId The ID of the area from which the destination is to be accessible.
     * @param toAreaId The ID of the destination area.
     */
    public void connectAreas(int fromAreaId, int toAreaId){
        if (!areas.containsKey(fromAreaId) || !areas.containsKey(toAreaId))
            System.out.println("One of the input areas doesn't exist!");
        else if (getArea(fromAreaId).getAdjacentAreas().contains(toAreaId))
            System.out.println("The connection has been added before.");
        else
            getArea(fromAreaId).addAdjacentArea(toAreaId);
    }

    /**
     * Checks if the given path obeys the one-way system.
     * @param areaIds The path is provided as a list of area IDs. It starts with the area ID at index 0.
     * @return Returns true iff visitors are allowed to visit the areas in the order given by the passed in list.
     */
    public boolean isPathAllowed(ArrayList<Integer> areaIds){
        for (int i = 0; i < areaIds.size()-1; i++){
            if (!getArea(areaIds.get(i)).getAdjacentAreas().contains(areaIds.get(i+1))){
                    return false;
            }
        }
        return true;
    }

    /**
     * Visits the areas in the specified order and records the names of all animals seen.
     * @param areaIdsVisited Areas IDs in the order they were visited.
     * @return Returns a list of the names of all animals seen during the visit in the order they were seen.
     */
    public ArrayList<String> visit(ArrayList<Integer> areaIdsVisited){
        var note = new ArrayList<String>();
        if (!isPathAllowed(areaIdsVisited)){
            return null;
        } else {
            for (int id : areaIdsVisited){
                var currentArea = getArea(id);
                for (Animal a : areas.get(id).get(currentArea)){
                    note.add(a.getNickname());
                }
            }
        }
        return note;
    }

    /**
     * Iterate recursively through and record every area connected with
     * the initial entrance and other areas connected to them.
     * @param areaId Areas IDs that starting area is connected to.
     */
    private void iterator(int areaId){
        for (Integer id : getArea(areaId).getAdjacentAreas()){
            if (id == 0 || reachableAreas.contains(id)){
                continue;
            }
            reachableAreas.add(id);
            iterator(id);
        }
    }

    /**
     * Finds all areas that cannot be reached from the entrance of the zoo.
     * @return The IDs of all inaccessible areas (unordered).
     */
    public ArrayList<Integer> findUnreachableAreas(){
        reachableAreas.clear();
        iterator(0);
        ArrayList<Integer> unreachableAreas = new ArrayList<>();
        for (Integer id : areas.keySet()){
            if (!reachableAreas.contains(id) && id != 0){
                unreachableAreas.add(id);
            }
        }
        return unreachableAreas;
    }

    /**
     * Sets a new ticket price in pounds and pence.
     * @param pounds The first part of the cost before the point e.g. 17 for a ticket that costs £17.50
     * @param pence The second part of the cost after the point e.g. 50 for a ticket that costs £17.50
     */
    public void setEntranceFee(int pounds, int pence){
        entranceFee.put("Pounds", pounds);
        entranceFee.put("Pence", pence);
    }

    /**
     * Stocks the ticket machine with the provided pool of cash.
     * @param coins The number of notes and coins of different denominations available.
     */
    public void setCashSupply(ICashCount coins){
        cashCount = coins;
    }

    /**
     * Used to inspect the ticket machine's currently available pool of cash.
     * @return The amount of each note and coin currently in the machine.
     */
    public ICashCount getCashSupply(){
        return cashCount;
    }

    /**
     * Used to create an ICashCount object with 0 notes and coins.
     * @return object with no coins and notes.
     */
    private ICashCount zeroChange(){
        var zero = new CashCount();
        zero.setNrNotes_20pounds(0);
        zero.setNrNotes_10pounds(0);
        zero.setNrNotes_5pounds(0);
        zero.setNrCoins_2pounds(0);
        zero.setNrCoins_1pound(0);
        zero.setNrCoins_50p(0);
        zero.setNrCoins_20p(0);
        zero.setNrCoins_10p(0);
        return zero;
    }

    /**
     * Convert an ICashCount object to a more readable {"Pounds": x, "Pence": y} format.
     * @param cash An ICashCount object that would be converted.
     * @return Returns a HashMap including the exact integer amount of Pounds and pence in the input object.
     */
    private HashMap<String, Integer> convertAmount(ICashCount cash){
        int pounds = 20 * cash.getNrNotes_20pounds() + 10 * cash.getNrNotes_10pounds()
                    + 5 * cash.getNrNotes_5pounds() + 2 * cash.getNrCoins_2pounds()
                    + cash.getNrCoins_1pound();
        int pence = 50 * cash.getNrCoins_50p() + 20 * cash.getNrCoins_20p()
                    + 10 * cash.getNrCoins_10p();
        if (pence >= 100){
            int mod = pence % 100;
            pounds = pounds + (pence - mod) / 100;
            pence = mod;
        }
        HashMap<String, Integer> converted = new HashMap<>();
        converted.put("Pounds", pounds);
        converted.put("Pence", pence);
        return converted;
    }

    /**
     * Calculate the amount of change of a payment (can be negative)
     * @param inserted The inserted cash.
     * @return Returns the change of the payment in {"Pounds": x, "Pence": y} format.
     */
    private HashMap<String, Integer> changeAmount(ICashCount inserted){
        var converted = convertAmount(inserted);
        var change = new HashMap<String, Integer>();
        int pounds = converted.get("Pounds") - entranceFee.get("Pounds");
        int pence = converted.get("Pence") - entranceFee.get("Pence");
        if (pence < 0){
            int mod = Math.abs(pence) % 100;
            pounds = pounds - 1 - (Math.abs(pence) - mod) / 100;
            pence = 100 - mod;
        }
        change.put("Pounds", pounds);
        change.put("Pence", pence);
        return change;
    }

    /**
     * Get the number of notes and coins in the machine using an int instead of a separate function call
     * @param cashSupply The cash stored in the vending machine. (cashCount)
     * @return Return the number of selected coins or notes.
     */
    private int getNrPounds(ICashCount cashSupply, int size){
        if (size == 20){
            return cashSupply.getNrNotes_20pounds();
        }
        if (size == 10){
            return cashSupply.getNrNotes_10pounds();
        }
        if (size == 5){
            return cashSupply.getNrNotes_5pounds();
        }
        if (size == 2){
            return cashSupply.getNrCoins_2pounds();
        }
        if (size == 1){
            return cashSupply.getNrCoins_1pound();
        }
        return 0;
    }

    /**
     * Get the number of notes and coins in the machine using an int instead of a separate function call.
     * @param cashSupply The cash stored in the vending machine. (cashCount)
     * @return Return the number of selected coins or notes.
     */
    private int getNrPence(ICashCount cashSupply, int size){
        if (size == 50){
            return cashSupply.getNrCoins_50p();
        }
        if (size == 20){
            return cashSupply.getNrCoins_20p();
        }
        if (size == 10){
            return cashSupply.getNrCoins_10p();
        }
        return 0;
    }

    /**
     * Use the highest value of Pounds possible to fit the given amount of Pounds.
     * @param changeAmount Amount of change needs to be returned.
     * @return Return a HashMap recording each type of Pounds the change contains.
     */
    private HashMap<Integer, Integer> fitPounds(HashMap<String, Integer> changeAmount) {
        var poundCounter = new HashMap<Integer, Integer>();
        int poundsAmount = changeAmount.get("Pounds");
        int penceAmount = changeAmount.get("Pence");
        int[] pounds = {20, 10, 5, 2, 1};
        for (Integer faceValue : pounds) {
            poundCounter.put(faceValue, 0);
            int nrPounds = getNrPounds(cashCount, faceValue);
            // fit remaining suitable notes as many as possible
            while (nrPounds != 0 && faceValue <= poundsAmount) {
                nrPounds -= 1;
                poundCounter.put(faceValue, poundCounter.get(faceValue) + 1);
                poundsAmount -= faceValue;
            }
        }
        // The left over pounds will be converted to pence and attempt to be fitted
        penceAmount += poundsAmount * 100;
        // 100 represents the number of pence
        poundCounter.put(100, penceAmount);
        return poundCounter;
    }

    /**
     * Use the highest value of Pence possible to fit the given amount of Pence.
     * @param changeAmount Amount of change needs to be returned.
     * @return Return a HashMap recording each type of notes or coins the change contains.
     */
    private HashMap<Integer, Integer> fitPence(HashMap<String, Integer> changeAmount){
        var penceCounter = new HashMap<Integer, Integer>();
        int penceAmount = fitPounds(changeAmount).get(100);
        int[] pence = {50, 20, 10};
        for (Integer faceValue : pence){
            penceCounter.put(faceValue, 0);
            int nrPence = getNrPence(cashCount, faceValue);
            while (nrPence != 0 && faceValue <= penceAmount){
                nrPence -= 1;
                penceCounter.put(faceValue, penceCounter.get(faceValue) + 1);
                penceAmount -= faceValue;
            }
        }
        return penceCounter;
    }

    /**
     * Calculate the best possible fit for change.
     * @param cashInserted The initial ICashCount object of inserted cash.
     * @return Return an ICashCount object containing the number of each type of notes or coins the change contains.
     */
    private ICashCount fitChange(ICashCount cashInserted){
        var calculatedChange = changeAmount(cashInserted);
        var poundsCounter = fitPounds(calculatedChange);
        var penceCounter = fitPence(calculatedChange);
        CashCount change = new CashCount();
        change.setNrNotes_20pounds(poundsCounter.get(20));
        change.setNrNotes_10pounds(poundsCounter.get(10));
        change.setNrNotes_5pounds(poundsCounter.get(5));
        change.setNrCoins_2pounds(poundsCounter.get(2));
        change.setNrCoins_1pound(poundsCounter.get(1));
        change.setNrCoins_50p(penceCounter.get(50));
        change.setNrCoins_20p(penceCounter.get(20));
        change.setNrCoins_10p(penceCounter.get(10));
        return change;
    }

    /**
     * Used for adding the inserted cash to the machine.
     * @param inserted The inserted cash.
     */
    private void addToMachine(ICashCount inserted){
        cashCount.setNrNotes_20pounds(cashCount.getNrNotes_20pounds() + inserted.getNrNotes_20pounds());
        cashCount.setNrNotes_10pounds(cashCount.getNrNotes_10pounds() + inserted.getNrNotes_10pounds());
        cashCount.setNrNotes_5pounds(cashCount.getNrNotes_5pounds() + inserted.getNrNotes_5pounds());
        cashCount.setNrCoins_2pounds(cashCount.getNrCoins_2pounds() + inserted.getNrCoins_2pounds());
        cashCount.setNrCoins_1pound(cashCount.getNrCoins_1pound() + inserted.getNrCoins_1pound());
        cashCount.setNrCoins_50p(cashCount.getNrCoins_50p() + inserted.getNrCoins_50p());
        cashCount.setNrCoins_20p(cashCount.getNrCoins_20p() + inserted.getNrCoins_20p());
        cashCount.setNrCoins_10p(cashCount.getNrCoins_10p() + inserted.getNrCoins_10p());
    }

    /**
     * Used for removing the inserted cash from the machine.
     * @param change The change returned to the ticket buyer.
     */
    private void removeFromMachine(ICashCount change){
        cashCount.setNrNotes_20pounds(cashCount.getNrNotes_20pounds() - change.getNrNotes_20pounds());
        cashCount.setNrNotes_10pounds(cashCount.getNrNotes_10pounds() - change.getNrNotes_10pounds());
        cashCount.setNrNotes_5pounds(cashCount.getNrNotes_5pounds() - change.getNrNotes_5pounds());
        cashCount.setNrCoins_2pounds(cashCount.getNrCoins_2pounds() - change.getNrCoins_2pounds());
        cashCount.setNrCoins_1pound(cashCount.getNrCoins_1pound() - change.getNrCoins_1pound());
        cashCount.setNrCoins_50p(cashCount.getNrCoins_50p() - change.getNrCoins_50p());
        cashCount.setNrCoins_20p(cashCount.getNrCoins_20p() - change.getNrCoins_20p());
        cashCount.setNrCoins_10p(cashCount.getNrCoins_10p() - change.getNrCoins_10p());
    }

    /**
     * Takes an amount of cash inserted into the ticket machine and returns the appropriate change
     * (if any) after deducting the amount of the entrance fee as set by @setEntranceFee.
     * @param cashInserted The notes and coins inserted by the user buying a ticket.
     * @return The change returned to the user (see assignment instructions for precise specification).
     */
    public ICashCount payEntranceFee(ICashCount cashInserted){
        ICashCount changeFit;
        // If the change is negative(the inserted amount is smaller than the ticket price)
        if (changeAmount(cashInserted).get("Pounds") < 0){
            return cashInserted;
        }
        // If the change is 0(the inserted amount is equal to the ticket price)
        else if (changeAmount(cashInserted).get("Pounds").equals(0)
                && changeAmount(cashInserted).get("Pence").equals(0)){
            addToMachine(cashInserted);
            return zeroChange();
        }
        // If the change is more than 0(the inserted amount is higher than the ticket price)
        else {
            addToMachine(cashInserted);
            changeFit = fitChange(cashInserted);
            var change = changeAmount(cashInserted);
            // If the machine cannot return the change due to the lack of notes and coins
            if (convertAmount(changeFit).get("Pounds") < change.get("Pounds")
               || (convertAmount(changeFit).get("Pounds").equals(change.get("Pounds"))
               && convertAmount(changeFit).get("Pence") < change.get("Pence"))){
                removeFromMachine(cashInserted);
                return cashInserted;
            }
        }
        // return the change
        return changeFit;
    }
}
