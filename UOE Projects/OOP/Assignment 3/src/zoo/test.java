package zoo;

import animals.Buzzard;
import animals.Lion;
import animals.Parrot;
import animals.Zebra;
import areas.Aquarium;
import areas.Cage;
import areas.Enclosure;
import dataStructures.CashCount;

import java.util.ArrayList;


public class test {
    public static void main(String[] args) {
        var test_area = new Cage(2);
        var aq = new Aquarium(2);
        var en = new Enclosure(3);
        var test_zoo = new Zoo();
        var p = new Parrot("Monke");
        var b = new Buzzard("Monke");
        var l = new Lion("Bruh");
        var z = new Zebra("Man");
        var cash = new CashCount();

        cash.setNrNotes_20pounds(0);
        cash.setNrNotes_10pounds(0);
        cash.setNrNotes_5pounds(0);
        cash.setNrCoins_2pounds(3);
        cash.setNrCoins_1pound(7);
        cash.setNrCoins_50p(0);
        cash.setNrCoins_20p(5);
        cash.setNrCoins_10p(5);

        //System.out.println(test_zoo.addArea(test_area));
        test_zoo.addArea(test_area);
        test_zoo.addArea(aq);
        test_zoo.addArea(en);

        test_zoo.connectAreas(0, 1);
        test_zoo.connectAreas(1, 2);
        test_zoo.connectAreas(1, 3);

        //test_zoo.connectAreas(3, 4);
        //test_zoo.connectAreas(4, 5);
        //test_zoo.connectAreas(5, 1);

        //System.out.println(test_zoo.addAnimal(1, p));
        System.out.println(test_zoo.addAnimal(1, p));
        System.out.println(test_zoo.addAnimal(1, b));
        System.out.println(test_zoo.addAnimal(3, l));
        System.out.println(test_zoo.addAnimal(3, z));

        test_zoo.setEntranceFee(3, 40);
        test_zoo.setCashSupply(cash);
        var cashInserted = new CashCount();
        cashInserted.setNrNotes_10pounds(2);
        var amount = test_zoo.payEntranceFee(cashInserted);
        //System.out.println(test_zoo.convertAmount(amount));

        //ArrayList<Integer> testPath = new ArrayList<>();
        //testPath.add(0);
        //testPath.add(2);
        //System.out.println(test_zoo.isPathAllowed(testPath));
        //System.out.println(test_zoo.findUnreachableAreas());
        //test_zoo.removeArea(1);
        //System.out.println(test_zoo.findUnreachableAreas());
    }
}
