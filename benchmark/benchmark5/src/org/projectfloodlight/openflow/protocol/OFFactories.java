// Copyright (c) 2008 The Board of Trustees of The Leland Stanford Junior University
// Copyright (c) 2011, 2012 Open Networking Foundation
// Copyright (c) 2012, 2013 Big Switch Networks, Inc.
// This library was generated by the LoxiGen Compiler.
// See the file LICENSE.txt which should have been included in the source distribution

// Automatically generated by LOXI from template of_factories.java
// Do not modify

package org.projectfloodlight.openflow.protocol;

//import org.projectfloodlight.openflow.protocol.*;
//import org.projectfloodlight.openflow.protocol.action.*;
//import org.projectfloodlight.openflow.protocol.actionid.*;
//import org.projectfloodlight.openflow.protocol.bsntlv.*;
//import org.projectfloodlight.openflow.protocol.errormsg.*;
//import org.projectfloodlight.openflow.protocol.meterband.*;
//import org.projectfloodlight.openflow.protocol.instruction.*;
//import org.projectfloodlight.openflow.protocol.instructionid.*;
//import org.projectfloodlight.openflow.protocol.match.*;
//import org.projectfloodlight.openflow.protocol.stat.*;
//import org.projectfloodlight.openflow.protocol.oxm.*;
//import org.projectfloodlight.openflow.protocol.oxs.*;
//import org.projectfloodlight.openflow.protocol.queueprop.*;
//import org.projectfloodlight.openflow.types.*;
//import org.projectfloodlight.openflow.util.*;
//import org.projectfloodlight.openflow.exceptions.*;
//import io.netty.buffer.ByteBuf;

public final class OFFactories {

//    private static final GenericReader GENERIC_READER = new GenericReader();

    public static OFFactory getFactory(OFVersion version) {
        switch(version) {
            case OF_10:
                return org.projectfloodlight.openflow.protocol.ver10.OFFactoryVer10.INSTANCE;
//            case OF_11:
//                return org.projectfloodlight.openflow.protocol.ver11.OFFactoryVer11.INSTANCE;
//            case OF_12:
//                return org.projectfloodlight.openflow.protocol.ver12.OFFactoryVer12.INSTANCE;
//            case OF_13:
//                return org.projectfloodlight.openflow.protocol.ver13.OFFactoryVer13.INSTANCE;
//            case OF_14:
//                return org.projectfloodlight.openflow.protocol.ver14.OFFactoryVer14.INSTANCE;
//            case OF_15:
//                return org.projectfloodlight.openflow.protocol.ver15.OFFactoryVer15.INSTANCE;
            default:
                throw new IllegalArgumentException("Unknown version: "+version);
        }
    }

    /*
    private static class GenericReader implements OFMessageReader<OFMessage> {
        public OFMessage readFrom(ByteBuf bb) throws OFParseError {
            if(!bb.isReadable())
                return null;
            short wireVersion = U8.f(bb.getByte(bb.readerIndex()));
            OFFactory factory;
            switch (wireVersion) {
                case 1:
                    factory = org.projectfloodlight.openflow.protocol.ver10.OFFactoryVer10.INSTANCE;
                    break;
                case 2:
                    factory = org.projectfloodlight.openflow.protocol.ver11.OFFactoryVer11.INSTANCE;
                    break;
                case 3:
                    factory = org.projectfloodlight.openflow.protocol.ver12.OFFactoryVer12.INSTANCE;
                    break;
                case 4:
                    factory = org.projectfloodlight.openflow.protocol.ver13.OFFactoryVer13.INSTANCE;
                    break;
                case 5:
                    factory = org.projectfloodlight.openflow.protocol.ver14.OFFactoryVer14.INSTANCE;
                    break;
                case 6:
                    factory = org.projectfloodlight.openflow.protocol.ver15.OFFactoryVer15.INSTANCE;
                    break;
                default:
                    throw new IllegalArgumentException("Unknown wire version: " + wireVersion);
            }
            return factory.getReader().readFrom(bb);
        }
    }

    public static OFMessageReader<OFMessage> getGenericReader() {
        return GENERIC_READER;
    }
    */
}
