package net.floodlightcontroller.linkdiscovery.internal;

import java.util.HashMap;
import java.util.Map;

import net.floodlightcontroller.core.IFloodlightProviderService;
import net.floodlightcontroller.core.IOFSwitch;

public class MockFloodlightProvider implements IFloodlightProviderService {
    @Override
    public Map<Long, IOFSwitch> getSwitches() {
        Map<Long, IOFSwitch> map = new HashMap<>();
        map.put(100L, new MockOFSwitch());
        return map;
    }
}
