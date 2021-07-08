package net.floodlightcontroller.learningswitch;

import net.floodlightcontroller.core.IOFSwitch;
import net.floodlightcontroller.core.internal.OFSwitch;
//import org.projectfloodlight.openflow.types.MacAddress;
import org.projectfloodlight.openflow.types.OFPort;
import org.projectfloodlight.openflow.types.VlanVid;

public class LearningSwitchTest {
    public static int main(int input) {
        // input: 0xFFFF, expected: 1, actual: 0
        // input: 100, expected: 1, actual: 1
        LearningSwitch ls = new LearningSwitch();
        IOFSwitch sw = new OFSwitch();
//        MacAddress mac = MacAddress.of(1);
        VlanVid vlan = (input == 0xFFFF) ? VlanVid.FULL_MASK : VlanVid.ofVlan(input);
        OFPort port = OFPort.of(2);
        ls.addToPortMap(sw, vlan, port);
        OFPort actual = ls.getFromPortMap(sw, vlan);
        if (actual != null && actual.equals(port)) {
            return 1;
        } else {
            return 0;
        }
    }
}
