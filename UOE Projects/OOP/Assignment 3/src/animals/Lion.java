package animals;

public class Lion extends Animal{

    private final String nickname;

    public Lion(String nickname){
        this.nickname = nickname;
    }

    public String getNickname(){
        return nickname;
    }

    public boolean isCompatibleWith(Animal animal){
        return animal instanceof Lion;
    }

    public String getHabitat(){
        return "Enclosure";
    }
}
