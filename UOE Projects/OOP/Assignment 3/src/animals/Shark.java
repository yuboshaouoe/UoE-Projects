package animals;

public class Shark extends Animal{

    private final String nickname;

    public Shark(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return !(animal instanceof Seal);
    }

    public String getHabitat(){
        return "Aquarium";
    }
}
