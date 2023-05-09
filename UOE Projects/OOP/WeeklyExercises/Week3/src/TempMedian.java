import java.util.Arrays;

public class TempMedian {

    private int[] temp_converter(String[] args){
        int[] temps = new int[args.length];
        int init_temp = Integer.parseInt(args[0]);
        temps[0] = init_temp;
        System.out.print(args[0] + " ");
        for (int i = 1; i < args.length; i++){
            if (args[i].equals(".")){
                temps[i] = init_temp;
            }
            else if (args[i].equals("+")){
                init_temp += 1;
                temps[i] = init_temp;
            }
            else {
                init_temp -= 1;
                temps[i] = init_temp;
            }
        }
        return temps;
    }

    private int[] sorter(String[] args){
        int[] arr = temp_converter(args);
        Arrays.sort(arr);
        return arr;
    }

    public void temp_median(String[] args){
        if
    }
}
