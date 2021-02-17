package net.floodlightcontroller.loadbalancer;

public class LoadBalancerTest {
    public static int main() {
        // expected: 0, actual: -1
        String input = "0"; // default pool id
        LoadBalancer lb = new LoadBalancer();
        LBPool pool = lb.createPool(null);
        LBVip vip = lb.createVip(null);
        pool.vipId = vip.id;
        lb.vips.remove(vip.id);
        int retVal = lb.removePool(input);
        return retVal;
    }
}