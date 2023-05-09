package animals;

public class Parrot extends Animal{

    private final String nickname;

    public Parrot(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return animal instanceof Parrot;
    }

    public String getHabitat(){
        return "Cage";
    }
}
