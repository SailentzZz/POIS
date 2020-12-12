import mirea.pois.arakelyan.Car;
import mirea.pois.arakelyan.CarInsurance;
import mirea.pois.arakelyan.Owner;
import org.junit.Test;
import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;

import java.util.Optional;

import static org.junit.Assert.assertEquals;

public class CarInsuranceRulesTest {

    @Test
    public void pois_lab_2_drools_rules_test() {
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

        assertEquals(2, ci.getRegionalMultiplier().intValue());
        assertEquals(1, ci.getAgeExpMultiplier().intValue());
        assertEquals(1, ci.getKOMultiplier().intValue());
        assertEquals(Optional.of(1.6), Optional.ofNullable(ci.getHPMultiplier()));
        assertEquals(ci.baseTariff, kSession.getGlobal("baseTariff"));
        assertEquals(0.8, ci.getCar().getOwner().getBonusMalus().doubleValue() - ci.getCar().getOwner().getBonusMalus().intValue(), 1);
        assertEquals(38157, ci.getFinalSum().intValue(), 100000);
    }
}
