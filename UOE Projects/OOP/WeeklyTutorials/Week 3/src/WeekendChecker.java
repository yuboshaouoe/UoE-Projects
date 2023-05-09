import java.util.Hashtable;

public class WeekendChecker {
    private final Hashtable<String, Integer> week;

    public WeekendChecker(){
        week = new Hashtable<>(7);
        week.put("Monday", 1);
        week.put("Tuesday", 2);
        week.put("Wednesday", 3);
        week.put("Thursday", 4);
        week.put("Friday", 5);
        week.put("Saturday", 6);
        week.put("Sunday", 0);
    }

    public void checker(String aWeekDay, int n){
        if ((week.get(aWeekDay) + n) % 7 == 0 || (week.get(aWeekDay) + n) % 7 == 6){
            System.out.println("Weekend");
        }
        else {
            System.out.println("not a weekend");
        }
    }

    public static void main(String[] args) {
        var test = new WeekendChecker();
        test.checker("Sunday", 7);
    }
}