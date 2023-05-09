////////////////////////////////////////////////////////////////
// Please note that there are deliberate errors in this file! //
////////////////////////////////////////////////////////////////
import java.util.Random;

/**
 * This class can be used to generate synthetic precipitation data for every day of any year.
 */
public class dataProvider
{
	/**
	 * An array of the minimum rainfall in a given month (in mm) indexed by month, zero based.
	 */
	public static final int[] minRainInMM = new int[]{5, 6, 7, 4, 2, 0, 0, 0, 2, 3, 5, 7};
	
	/**
	 * An array of the maximum rainfall in a given month (in mm)indexed by month, zero based.
	 */
	public static final int[] maxRainInMM = new int[]{35, 38, 40, 30, 15, 10, 12, 15, 25, 32, 35, 38};
	
	/**
	 * "Thirty days hath September,
	 * April, June, and November,
	 * All the rest have thirty-one,
	 * But February's twenty-eight,
	 * The leap year, which comes once in four,
	 * Gives February one day more.
	 */
	public static final int[] daysPerMonth = {
			31, // January
			28, // February
			31, // March
			30, // April
			31, // May
			30, // June
			31, // July
			31, // August
			30, // September
			31, // October
			30, // November
			31, // December
	};
	
	public static final Random rand = new Random();
	
	/**
	 * Creates and returns synthetically generated rainfall data given a
	 * pair of integers representing the month and a day within that month.
	 * @param month for which you want rain data (in the range [1, 12], inclusive)
	 * @param day   for which you want rain data within the specified month
	 *              (in the range [1, length of month], inclusive)
	 * @return the amount of rain (in mm) generated for the specified day
	 */
	public int getRain(int month, int day) {
		month = month - 1;
		if(month < 0 || month >= 12) {
			return -1;
		}
		if(day < 1 || day > daysPerMonth[month]) {
			return -1;
		}

		int    minRainThisMonth      = minRainInMM[month];
		int    maxRainThisMonth      = maxRainInMM[month];
		int    maxRainNextMonth      = minRainInMM[(month + 1) % 12];
		int    maxRainfallDifference = maxRainNextMonth - maxRainThisMonth;
		double distanceThroughMonth  = (double)day / (double)daysPerMonth[month];
		maxRainfallDifference = (int)(maxRainfallDifference * distanceThroughMonth);
		int maxRainToday = maxRainThisMonth + maxRainfallDifference;

		int rain = (int)(rand.nextFloat() * maxRainToday);
		if(rain < minRainThisMonth) {
			rain = minRainThisMonth;
		}
		return rain;
	}
}
