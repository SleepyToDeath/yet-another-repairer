package net.floodlightcontroller.loadbalancer;

public class LoadBalancerTest {
    public static int main() {
        LoadBalancer lb = new LoadBalancer();
        LBPool pool = lb.createPool(null);
        LBVip vip = lb.createVip(null);
        pool.vipId = vip.id;
        lb.vips.remove(vip.id); // retVal = 0 if this line is removed
        int retVal = lb.removePool(pool.id);
        return retVal;
    }
}