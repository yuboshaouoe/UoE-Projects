package areas;

import java.util.ArrayList;

public class Enclosure extends Habitat{
    private final int maxAnimal;
    private final ArrayList<Integer> adjacentArea = new ArrayList<>();

    public Enclosure(int maxAnimal){
        this.maxAnimal = maxAnimal;
    }

    public int getMax(){
        return maxAnimal;
    }

    public void addAdjacentArea(int areaId){
        adjacentArea.add(areaId);
    }

    public void removeAdjacentArea(int areaId){
        if (adjacentArea.contains(areaId))
            adjacentArea.remove(Integer.valueOf(areaId));
    }

    public ArrayList<Integer> getAdjacentAreas(){
        return adjacentArea;
    }
}
