package net.floodlightcontroller.dhcpserver;

import org.projectfloodlight.openflow.types.IPv4Address;

public class DHCPInstanceTest {
    public static int main(int input) {
        // input: 0, expected: 0, actual: 1
        // input: 1, expected: 1, actual: 1
        DHCPInstance instance = DHCPInstance.createInstance()
				.setServerID(IPv4Address.of(input))
                .build();
		return instance == null ? 0 : 1;
    }
}
