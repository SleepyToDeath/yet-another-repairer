package net.floodlightcontroller.util;

import org.projectfloodlight.openflow.protocol.OFVersion;
import org.projectfloodlight.openflow.protocol.action.OFAction;
import org.projectfloodlight.openflow.protocol.ver10.OFActionSetTpDstVer10;
import org.projectfloodlight.openflow.types.TransportPort;

public class ActionUtilsTest {
    public static int main(String input) {
        // input: "1", expected: 1, actual: 0
        // input: "2", expected: 0, actual: 0
        OFAction actual = ActionUtils.decode_set_dst_port(input, OFVersion.OF_10);
        OFAction expected = new OFActionSetTpDstVer10(TransportPort.of(1));
        return actual.equals(expected) ? 1 : 0;
    }
}
