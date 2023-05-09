package com.javalearning;

public class LopsidedNumberTriangle {

    public static void main(String[] args) {
        for (String arg : args){
            System.out.println(arg.repeat(Integer.parseInt(arg)));
        }
    }
}
