package animals;

public class Buzzard extends Animal{
    private final String nickname;

    public Buzzard(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return animal instanceof Buzzard;
    }

    public String getHabitat(){
        return "Cage";
    }
}
