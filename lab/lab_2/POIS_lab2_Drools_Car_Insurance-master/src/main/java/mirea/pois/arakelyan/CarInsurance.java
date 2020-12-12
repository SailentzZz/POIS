package mirea.pois.arakelyan;

import org.drools.core.base.RuleNameEndsWithAgendaFilter;
import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;

public class CarInsurance {
    public final Double baseTariff;
    private final Car car;
    private Double regionalMultiplier = 0.0;
    private Double ageExpMultiplier = 0.0;
    private Double kOMultiplier = 0.0;
    private Double hPMultiplier = 0.0;
    private Double finalSum;


    public CarInsurance(Double bt, Car c) {
        this.baseTariff = bt;
        this.car = c;
    }


    public Car getCar() {
        return car;
    }

    public Double getRegionalMultiplier() {
        return regionalMultiplier;
    }

    public void setRegionalMultiplier(Double regionalMultiplier) {
        this.regionalMultiplier = regionalMultiplier;
    }

    public void calculatePrice (){
        this.finalSum = baseTariff
                * regionalMultiplier
                * getCar().getOwner().getBonusMalus()
                * ageExpMultiplier
                * kOMultiplier
                * hPMultiplier
                * 1.0 * 1.5 * 1.0;
    }

    public Double getAgeExpMultiplier() {
        return ageExpMultiplier;
    }

    public void setAgeExpMultiplier(Double ageExpMultiplier) {
        this.ageExpMultiplier = ageExpMultiplier;
    }

    public Double getKOMultiplier() {
        return kOMultiplier;
    }

    public void setKOMultiplier(Double kOMultiplier) {
        this.kOMultiplier = kOMultiplier;
    }

    public Double getHPMultiplier() {
        return hPMultiplier;
    }

    public void setHPMultiplier(Double hPMultiplier) {
        this.hPMultiplier = hPMultiplier;
    }

    public Double getFinalSum() {
        return finalSum;
    }

    public Boolean areMultipliersAvailable() {
        return this.kOMultiplier > 0.0 &&
                this.ageExpMultiplier > 0.0 &&
                this.hPMultiplier > 0.0 &&
                this.regionalMultiplier > 0.0;
    }

    public static final void main(String[] args) {
        try {
            KieServices ks = KieServices.Factory.get();
            KieContainer kContainer = ks.getKieClasspathContainer();
            KieSession kSession = kContainer.newKieSession("ksession-rules");

            CarInsurance ci = new CarInsurance(10000.0, new Car(
                    new Owner(99, 25, 5),
                    1,
                    500));

            kSession.insert(ci);
            kSession.setGlobal("baseTariff", ci.baseTariff);
            kSession.fireAllRules();

        } catch (Throwable t){
            t.printStackTrace();
        }
    }

}
