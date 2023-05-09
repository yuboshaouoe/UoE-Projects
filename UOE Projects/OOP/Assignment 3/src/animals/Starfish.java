package animals;

public class Starfish extends Animal{

    private final String nickname;

    public Starfish(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return true;
    }

    public String getHabitat(){
        return "Aquarium";
    }
}
