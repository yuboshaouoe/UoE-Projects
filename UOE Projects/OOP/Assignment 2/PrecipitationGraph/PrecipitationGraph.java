////////////////////////////////////////////////////////////////
// Please note that there are deliberate errors in this file! //
////////////////////////////////////////////////////////////////
import java.text.DecimalFormat;

public class PrecipitationGraph
{
	// This DecimalFormat is used to print single digit day/month numbers with a leading zero.
	public static DecimalFormat doubleDigitFormat = new DecimalFormat("#00");
	
	// An instance of DataProvider
	dataProvider dataProvider = new dataProvider();
	
	// A 2D array to store information obtained from the getRain function, indexed as [month][year].
	private int[][] rainfall;
	
	// The names of the months of the year
	private final String[] monthName =
	{
		"January  ",
		"February ",
		"March    ",
		"April    ",
		"May      ",
		"June     ",
		"July     ",
		"August   ",
		"September",
		"October  ",
		"November ",
		"December "
	};
	
	// The number of days in each month.
	private static final int[] DAYS_PER_MONTH =
	{
		31, // January
		29, // February
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
	
	// The default scale is 1, meaning that each mm of rain is represented by one star.
	private float scale = 1.0f;
	
	PrecipitationGraph()
	{
		// Initialise the rainfall array with values for every day of the year.
		rainfall = new int[12][];
		for(int month = 0; month < 12; month++)
		{
			// Allow for different month lengths
			rainfall[month] = new int[DAYS_PER_MONTH[month]];
			
			// Fill the rainfall array with information from the DataProvider.
			for(int day = 0; day < DAYS_PER_MONTH[month]; day++)
				rainfall[month][day] = dataProvider.getRain(day + 1, month + 1);
		}
	}
	
	private int monthlyAverage(int month)
	{
		int sum = 0;
		
		// Add up the rain of every day of the month
		for(int day = 0; day < DAYS_PER_MONTH[month]; day++)
		{
			sum += rainfall[month][day];
		}
		// Return the average rainfall per day
		return (int)(sum / DAYS_PER_MONTH[month]);
	}
	
	private String starString(int amountOfRain)
	{
		String stars = "";
		for(int i = 0; i < amountOfRain; i++)
		{
			stars += "* ";	// Adds a star for every mm of rain
		}
		return stars;
	}
	
	private int findMax(int[] array)
	{
		// This is like a temporary variable which does massively complicated stuff in a loop
		// Someone was saying its real efficient to sort of set it to like the first thing in the array
		// but then I was like no why make it complicated so Im just using a zero Anyway its kinda
		// working so why bother change it right?
		int temp = 0;
		for(int i = 0; i < array.length; i++)
		{
			// If a bigger entry is found, remember it
			if(array[i] > temp)
			{
				temp = array[i];
			}
		}
		return temp;
	}
	
	/* *************** GET AND SET SCALE *************** */
	public float getScale()
	{
		// Returns the scale.
		return scale;
	}
	
	public void setScale(float scale)
	{
		this.scale = scale;	// Set the scale and print a confirmation.
		System.out.println("The scale was set to : " + scale + "\n");
	}
	
	/* *************** PREPARE DATA *************** */
	private int[] preparedData()
	{
		int[] year = new int[12];
		for(int month = 0; month < 12; month++)
		{
			year[month] = (int)(scale * monthlyAverage(month));
		}
		return year;
	}
	
	private int[] preparedData(int[] month)
	{
		int[] array = month.clone();
		for(int day = 0; day < month.length; day++)
		{
			array[day] = (int)(scale * month[day]);
		}
		return array;
	}
	
	/* ******************************* PRINTING METHODS ******************************* */
	
	private void horizontalGraph(int[] array)
	{
		// Go through every entry in the array:
		for(int i = 0; i < array.length; i++)
		{
			// Print whatever the starString method returns.
			System.out.println(doubleDigitFormat.format(i + 1) + " |  " + starString(array[i]));
		}
		System.out.println("\n");
	}
	
	private void verticalGraph(int[] array)
	{
		String stars;
		
		// Get the largest entry in the given array.
		int ArrayMax = findMax(array);
		
		// Print each row of stars representing the amount of rainfall.
		for(int i = ArrayMax; i > 0; i--)
		{
			// Every new line starts with a bar (this also resets the previous String).
			stars = "|";
			
			// For every entry in the array:
			for(int j = 0; j < array.length; j++)
			{
				// If the value stored in the array is >= the current line height
				if(array[j] >= i)
				{
					stars += " * ";   // then add a star
				}
				else
				{
					stars += "   ";   // add a blank
				}
			}
			// Print the row of stars
			System.out.println(stars);
		}
		
		// All lines start with a bar.
		String dottedLine = "|";
		String dateLine = "|";
		
		for(int i = 0; i < array.length; i++)
		{
			// For every entry make the dotted line longer
			dottedLine += "---";
			
			// Add a number for each entry
			dateLine += (doubleDigitFormat.format(i + 1) + " ");
		}
		
		// Print the dottedLine
		System.out.println(dottedLine);
		
		// Print the dateLine
		System.out.println(dateLine);
		
		// Print a new line
		System.out.println("\n");
	}
	
	public void monthHorizontal(int month)
	{
		// Print the graph heading.
		System.out.println(monthName[month] + " Scale : " + scale + " (one entry per day) :");
		
		// Call horizontalGraph with the data for the given month.
		horizontalGraph(preparedData(rainfall[month]));
	}
	
	public void yearHorizontal()
	{
		// Print the graph heading.
		System.out.println("Year (One entry per month) Scale : " + scale + " :");
		
		// Call horizontalGraph with the data for the whole year.
		horizontalGraph(preparedData());
	}
	
	public void monthVertical(int month)
	{
		// Print the graph heading.
		System.out.println(monthName[month] + " Scale : " + scale + " (one entry per day) :");
		
		// Call verticalGraph with the data for the given month.
		this.verticalGraph(preparedData(rainfall[month]));
	}
	
	public void yearVertical()
	{
		// Print the graph heading.
		System.out.println("Year (One entry per month) Scale : " + scale + " :");
		
		// Call verticalGraph with the data for the whole year.
		this.verticalGraph(preparedData());
	}
}
