package net.floodlightcontroller.simpleft;

import net.floodlightcontroller.core.IOFSwitch;
import net.floodlightcontroller.core.internal.IOFSwitchService;
import org.projectfloodlight.openflow.types.DatapathId;

import java.util.Set;

public class MockSwitchService implements IOFSwitchService {

    private IOFSwitch sw;

    public MockSwitchService(DatapathId dpid) {
        sw = new MockSwitch(dpid);
    }

    @Override
    public IOFSwitch getActiveSwitch(DatapathId dpid) {
        if (dpid.equals(sw.getId())) {
            return sw;
        } else {
            return null;
        }
    }

    @Override
    public Set<DatapathId> getAllSwitchDpids() {
        return null;
    }

}
