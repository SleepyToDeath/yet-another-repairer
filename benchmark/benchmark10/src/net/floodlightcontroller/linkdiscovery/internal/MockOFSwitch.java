package net.floodlightcontroller.linkdiscovery.internal;

import net.floodlightcontroller.core.IOFSwitch;
import org.openflow.protocol.OFPhysicalPort;

public class MockOFSwitch implements IOFSwitch {
    @Override
    public OFPhysicalPort getPort(short portNumber) {
        OFPhysicalPort port = new OFPhysicalPort();
        port.setPortNumber(portNumber);
        return port;
    }
}
