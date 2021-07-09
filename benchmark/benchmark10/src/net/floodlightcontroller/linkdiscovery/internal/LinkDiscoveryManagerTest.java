package net.floodlightcontroller.linkdiscovery.internal;

import net.floodlightcontroller.core.IListener.Command;
import net.floodlightcontroller.packet.LLDP;
import net.floodlightcontroller.packet.LLDPTLV;

public class LinkDiscoveryManagerTest {
    public static int main(short input) {
        // input: 1, expected: 1, actual: 0
        // input: 3, expected: 1, actual: 1
        LinkDiscoveryManager manager = new LinkDiscoveryManager();
        manager.floodlightProvider = new MockFloodlightProvider();
        LLDPTLV lldptlv = new LLDPTLV();
        lldptlv.setLength(input);
        LLDP lldp = new LLDP();
        lldp.setPortId(lldptlv);
        String sw = "100"; // hard-coded
        Command cmd = manager.handleLldp(lldp, sw, (short) 2, true, null);
        return cmd == Command.STOP ? 1 : 0;
    }
}
