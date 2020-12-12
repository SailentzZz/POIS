package mirea.pois.arakelyan;

import java.util.Random;

public class Owner {
    private final Integer regionNumber;
    private final Double bonusMalus;
    private final Integer age;
    private final Integer experience;


    public Owner(Integer regionNumber, Integer age, Integer experience) {
        Random r = new Random();
        this.regionNumber = regionNumber;
        this.bonusMalus = 0.5 + (2.45 - 0.5) * r.nextDouble();
        this.age = age;
        this.experience = experience;
    }

    public Integer getRegionNumber() {
        return regionNumber;
    }

    public Double getBonusMalus() {
        return bonusMalus;
    }

    public Integer getAge() {
        return age;
    }

    public Integer getExperience() {
        return experience;
    }
}
