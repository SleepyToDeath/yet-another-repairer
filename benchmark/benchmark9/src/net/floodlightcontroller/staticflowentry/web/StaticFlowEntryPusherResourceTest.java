package net.floodlightcontroller.staticflowentry.web;

import net.floodlightcontroller.staticflowentry.StaticFlowEntryPusher;

import java.util.HashMap;
import java.util.Map;

public class StaticFlowEntryPusherResourceTest {
    public static int main(String input) {
        // input: "1", expected: 3, actual: -1
        // input: "2", expected: 3, actual: -1
        Map<String, Object> rows = new HashMap<>();
        rows.put(StaticFlowEntryPusher.COLUMN_ICMP6_TYPE, "0x88");
        rows.put(StaticFlowEntryPusher.COLUMN_ICMP_TYPE, input);
        StaticFlowEntryPusherResource resource = new StaticFlowEntryPusherResource();
        return resource.checkFlow(rows);
    }
}