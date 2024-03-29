package net.floodlightcontroller.firewall;

import org.projectfloodlight.openflow.types.MacAddress;

public class FirewallRuleTest {
    public static int main(int input) {
        // input: 100, expected: 1, actual: 0
        // input: 200, expected: 0, actual: 0
        FirewallRule rule1 = new FirewallRule();
        rule1.dl_dst = MacAddress.of(100);
        rule1.any_dl_dst = false;
        FirewallRule rule2 = new FirewallRule();
        rule2.dl_dst = MacAddress.of(input);
        rule2.any_dl_dst = false;
        return rule1.isSameAs(rule2) ? 1 : 0;
    }
}