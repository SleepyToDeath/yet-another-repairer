package net.floodlightcontroller.simpleft;

import org.projectfloodlight.openflow.types.DatapathId;

public class FTTest {
    public static int main(int input) {
        // input: 100, expected: 0, actual: 0
        // input: 10, expected: 0, actual: 1
        FT ft = new FT();
        ft.switchActivated(DatapathId.of(input));
        return ft.storeFT.getErrorStatus() ? 1 : 0;
    }
}
