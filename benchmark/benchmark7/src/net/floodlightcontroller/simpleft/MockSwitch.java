package net.floodlightcontroller.simpleft;

import net.floodlightcontroller.core.IOFSwitch;
import org.projectfloodlight.openflow.types.DatapathId;

public class MockSwitch implements IOFSwitch {

    private DatapathId id;

    public MockSwitch(DatapathId id) {
        this.id = id;
    }

    @Override
    public DatapathId getId() {
        return id;
    }

    @Override
    public boolean isActive() {
        return true;
    }

    @Override
    public String toString() {
        return "MockId";
    }
}
