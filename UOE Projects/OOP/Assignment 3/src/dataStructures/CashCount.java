package dataStructures;

public class CashCount implements ICashCount{
    private int _20pounds;
    private int _10pounds;
    private int _5pounds;
    private int _2pounds;
    private int _1pound;
    private int _50p;
    private int _20p;
    private int _10p;

    // ---------------- SETTERS ----------------

    public void setNrNotes_20pounds(int noteCount){
        _20pounds = noteCount;
    }

    public void setNrNotes_10pounds(int noteCount){
        _10pounds = noteCount;
    }

    public void setNrNotes_5pounds(int noteCount){
        _5pounds = noteCount;
    }

    public void setNrCoins_2pounds(int coinCount){
        _2pounds = coinCount;
    }

    public void setNrCoins_1pound(int coinCount){
        _1pound = coinCount;
    }

    public void setNrCoins_50p(int coinCount){
        _50p = coinCount;
    }

    public void setNrCoins_20p(int coinCount){
        _20p = coinCount;
    }

    public void setNrCoins_10p(int coinCount){
        _10p = coinCount;
    }

    // ---------------- GETTERS ----------------

    public int getNrNotes_20pounds(){
        return _20pounds;
    }

    public int getNrNotes_10pounds(){
        return _10pounds;
    }

    public int getNrNotes_5pounds(){
        return _5pounds;
    }

    public int getNrCoins_2pounds(){
        return _2pounds;
    }

    public int getNrCoins_1pound(){
        return _1pound;
    }

    public int getNrCoins_50p(){
        return _50p;
    }

    public int getNrCoins_20p(){
        return _20p;
    }

    public int getNrCoins_10p(){
        return _10p;
    }
}
