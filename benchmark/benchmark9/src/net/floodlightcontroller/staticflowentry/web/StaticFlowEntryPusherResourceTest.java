package net.floodlightcontroller.staticflowentry.web;

import net.floodlightcontroller.staticflowentry.StaticFlowEntryPusher;

import java.util.HashMap;

public class StaticFlowEntryPusherResourceTest {
    public static int main(int inputv4, int inputv6) {
        // input: 135, 0,  expected: 3, actual: -1
        // input: 136, 0,  expected: 3, actual: -1
        // input: 0, 88,  expected: 3, actual: 3
        HashMap<String, Integer> rows = new HashMap<>();
        rows.put(StaticFlowEntryPusher.COLUMN_ICMP6_TYPE, new Integer(inputv6));
        rows.put(StaticFlowEntryPusher.COLUMN_ICMP_TYPE, new Integer(inputv4));
        StaticFlowEntryPusherResource resource = new StaticFlowEntryPusherResource();
        return resource.checkFlow(rows);
    }
}
