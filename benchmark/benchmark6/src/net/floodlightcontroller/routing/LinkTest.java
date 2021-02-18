package net.floodlightcontroller.routing;

import org.projectfloodlight.openflow.types.DatapathId;
import org.projectfloodlight.openflow.types.OFPort;

public class LinkTest {
    public static int main(int input) {
        // input: 2, expected: 0, actual: 0
        // input: 5, expected: 3, actual: 0
        Link link1 = new Link(DatapathId.NONE, OFPort.of(2), DatapathId.NONE, OFPort.of(4));
        Link link2 = new Link(DatapathId.NONE, OFPort.of(input), DatapathId.NONE, OFPort.of(4));
        return link2.compareTo(link1);
    }
}
