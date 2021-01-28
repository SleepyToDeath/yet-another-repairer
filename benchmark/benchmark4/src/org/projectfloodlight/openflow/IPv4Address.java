package org.projectfloodlight.openflow.types;

//import io.netty.buffer.ByteBuf;

//import static com.google.common.base.Preconditions.checkArgument;

//import java.net.Inet4Address;
//import java.net.InetAddress;
//import java.net.UnknownHostException;
//import java.util.Arrays;
//
//import javax.annotation.Nonnull;
//
//import org.projectfloodlight.openflow.exceptions.OFParseError;
//import org.projectfloodlight.openflow.protocol.OFMessageReader;
////import org.projectfloodlight.openflow.protocol.Writeable;

//import com.google.common.base.Preconditions;
//import com.google.common.hash.PrimitiveSink;
//import com.google.common.primitives.UnsignedInts;

/**
 * Wrapper around an IPv4Address address
 *
 * @author Andreas Wundsam {@literal <}andreas.wundsam@bigswitch.com{@literal >}
 */
//public class IPv4Address extends IPAddress<IPv4Address> implements Writeable {
public class IPv4Address {
    //    static final int LENGTH = 4;
    private final int rawValue;

//    private static final int NOT_A_CIDR_MASK = -1;
//    private static final int CIDR_MASK_CACHE_UNSET = -2;
    // Must appear before the static IPv4Address constant assignments
//    private volatile int cidrMaskLengthCache = CIDR_MASK_CACHE_UNSET;

    private final static int NONE_VAL = 0x0;
    public final static IPv4Address NONE = new IPv4Address(NONE_VAL);

//    public static final IPv4Address NO_MASK = IPv4Address.of(0xFFFFFFFF);
//    public static final IPv4Address FULL_MASK = IPv4Address.of(0x00000000);

    private IPv4Address(final int rawValue) {
        this.rawValue = rawValue;
    }

    public static IPv4Address of(final int raw) {
        return new IPv4Address(raw);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
//[TODO]
//        if (getClass() != obj.getClass())
//            return false;
        IPv4Address other = (IPv4Address) obj;
        if (rawValue != other.rawValue)
            return false;
        return true;
    }
}
