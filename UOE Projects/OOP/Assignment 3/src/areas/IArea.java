package areas;

import java.util.ArrayList;

/**
 * This file must remain exactly as it is.
 */
public interface IArea
{
	/**
	 * @return Returns the IDs of the areas adjacent to this one.
	 */
	public ArrayList<Integer> getAdjacentAreas();

	/**
	 * @param areaId the ID of the area to be connected to.
	 */
	void addAdjacentArea(int areaId);

	/**
	 * @param areaId the ID of the area to be removed.
	 */
	void removeAdjacentArea(int areaId);
}
