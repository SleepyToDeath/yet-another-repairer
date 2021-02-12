package net.floodlightcontroller.dhcpserver;

import org.projectfloodlight.openflow.types.IPv4Address;

public class DHCPInstanceTest {
    public static int main() {
        // expected: 0, actual: 1
        int input = 0;
        /* Create DHCP Instance */
        DHCPInstance instance = DHCPInstance.createInstance()
				.setServerID(IPv4Address.of(input))
                .build();
		return instance == null ? 0 : 1;
    }
}
