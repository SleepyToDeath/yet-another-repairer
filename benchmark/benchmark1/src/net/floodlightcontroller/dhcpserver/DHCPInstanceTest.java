package net.floodlightcontroller.dhcpserver;

import org.projectfloodlight.openflow.types.IPv4Address;

public class DHCPInstanceTest {
    public static int main() {
        /* Create DHCP Instance */
        DHCPInstance instance = DHCPInstance.createInstance()
				.setServerID(IPv4Address.of(0))
                .build();

		if (instance == null)
			return 0;
		else
			return 1;
    }
}
