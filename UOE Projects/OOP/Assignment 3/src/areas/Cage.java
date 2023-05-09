package areas;

import java.util.ArrayList;

public class Cage extends Habitat{
    private final int maxAnimal;
    private final ArrayList<Integer> adjacentArea = new ArrayList<>();

    public Cage(int maxAnimal){
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
