package com.javalearning;

public class Multiplier {

    public static void main(String[] args) {
	    int init = 1;
	    int[] numbers = new int[args.length];
	    for (int i = 0; i < args.length; i++){
	        numbers[i] = Integer.parseInt(args[i]);
        }
	    for (int n : numbers){
	        init *= n;
        }
        System.out.println(init);
    }
}
