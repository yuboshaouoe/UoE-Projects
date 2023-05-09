public class Test
{
	public static void main(String[] args)
	{
		PrecipitationGraph graph = new PrecipitationGraph();
		
		// Test the horizontal graph for displaying yearly precipitation.
		graph.yearHorizontal();
		
		// Now see if the vertical graph shows the same information.
		graph.yearVertical();
		
		// Precipitation for the month of January.
		graph.monthVertical(1);
		
		// Check if scaling correctly rounds the data to the nearest integer (half up).
		graph.setScale(0.5f);
		graph.yearHorizontal();
	}
}
