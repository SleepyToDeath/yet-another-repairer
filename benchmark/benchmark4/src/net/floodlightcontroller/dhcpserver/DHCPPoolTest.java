package net.floodlightcontroller.dhcpserver;

import org.projectfloodlight.openflow.types.IPv4Address;
import org.projectfloodlight.openflow.types.MacAddress;

import java.util.Optional;

public class DHCPPoolTest {
    public static int main() {
        // expected: 1, actual: 0
        IPv4Address ip = IPv4Address.of(1);
        int size = 10;
        DHCPPool pool = new DHCPPool(ip, size);

        MacAddress mac = MacAddress.of(100);
        Optional<IPv4Address> addr = pool.getLeaseIP(mac);
        if (addr == null || addr.isPresent()) {
            return 0;
        } else {
            return 1;
        }
    }
}