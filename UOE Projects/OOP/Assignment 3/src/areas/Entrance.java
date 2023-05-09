package areas;

import java.util.ArrayList;

public class Entrance extends NonHabitat{
    private final ArrayList<Integer> adjacentArea = new ArrayList<>();

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
