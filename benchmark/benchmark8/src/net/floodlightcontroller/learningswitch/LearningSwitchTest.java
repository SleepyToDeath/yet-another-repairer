package net.floodlightcontroller.learningswitch;

import net.floodlightcontroller.core.IOFSwitch;
import net.floodlightcontroller.core.internal.OFSwitch;
import org.projectfloodlight.openflow.types.MacAddress;
import org.projectfloodlight.openflow.types.OFPort;
import org.projectfloodlight.openflow.types.VlanVid;

public class LearningSwitchTest {
    public static int main() {
        // expected: 1, actual: 0
        LearningSwitch ls = new LearningSwitch();
        IOFSwitch sw = new OFSwitch();
        MacAddress mac = MacAddress.of(1);
        VlanVid vlan = VlanVid.FULL_MASK;
        OFPort port = OFPort.of(2);
        ls.addToPortMap(sw, mac, vlan, port);
        OFPort actual = ls.getFromPortMap(sw, mac, vlan);
        if (actual != null && actual.equals(port)) {
            return 1;
        } else {
            return 0;
        }
    }
    public static void main(String[] args) {
        System.out.println(main());
    }
}
