package net.floodlightcontroller.firewall;

import org.projectfloodlight.openflow.types.MacAddress;

public class FirewallRuleTest {
    public static int main() {
        FirewallRule rule1 = new FirewallRule();
        rule1.dl_dst = MacAddress.of(100);
        rule1.any_dl_dst = false;
        FirewallRule rule2 = new FirewallRule();
        rule2.dl_dst = MacAddress.of(100);
        rule2.any_dl_dst = false;
        return rule1.isSameAs(rule2) ? 1 : 0;
    }
}