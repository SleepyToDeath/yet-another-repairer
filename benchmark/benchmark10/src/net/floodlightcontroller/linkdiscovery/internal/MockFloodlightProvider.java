package net.floodlightcontroller.linkdiscovery.internal;

import java.util.HashMap;

import net.floodlightcontroller.core.IFloodlightProviderService;
import net.floodlightcontroller.core.IOFSwitch;

public class MockFloodlightProvider implements IFloodlightProviderService {
    @Override
    public HashMap<String, IOFSwitch> getSwitches() {
        HashMap<String, IOFSwitch> map = new HashMap<>();
        map.put("100", new MockOFSwitch());
        return map;
    }
}
