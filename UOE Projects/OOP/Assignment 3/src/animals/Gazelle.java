package animals;

public class Gazelle extends Animal{

    private final String nickname;

    public Gazelle(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return !(animal instanceof Lion);
    }

    public String getHabitat(){
        return "Enclosure";
    }
}
