package net.floodlightcontroller.util;

import org.projectfloodlight.openflow.protocol.OFVersion;
import org.projectfloodlight.openflow.protocol.action.OFAction;
import org.projectfloodlight.openflow.protocol.ver10.OFActionSetTpDstVer10;
import org.projectfloodlight.openflow.types.TransportPort;

public class ActionUtilsTest {
    public static int main() {
        // expected: 1, actual: 0
        OFAction actual = ActionUtils.decode_set_dst_port("1", OFVersion.OF_10);
        OFAction expected = new OFActionSetTpDstVer10(TransportPort.of(1));
        return actual.equals(expected) ? 1 : 0;
    }
}
