package mirea.pois.arakelyan;

public class Car {
    private Owner owner;
    private Integer allowedOwners;
    private Integer hp;

    public Car (Owner owner, Integer allowedOwners, Integer hp){
        this.owner = owner;
        this.allowedOwners = allowedOwners;
        this.hp = hp;
    }

    public Owner getOwner() {
        return owner;
    }

    public Integer getAllowedOwners() {
        return allowedOwners;
    }

    public Integer getHp() {
        return hp;
    }

    public void setHp(Integer hp) {
        this.hp = hp;
    }
}
