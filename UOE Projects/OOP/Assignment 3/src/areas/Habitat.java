package areas;

import java.util.ArrayList;

public abstract class Habitat implements IArea{

    public abstract int getMax();

    public abstract ArrayList<Integer> getAdjacentAreas();
}
