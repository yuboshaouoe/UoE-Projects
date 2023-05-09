package animals;

public class Seal extends Animal{

    private final String nickname;

    public Seal(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return !(animal instanceof Shark);
    }

    public String getHabitat(){
        return "Aquarium";
    }
}
