package net.floodlightcontroller.simpleft;

//import java.util.ArrayList;
//import java.util.Collection;
import java.util.Iterator;
//import java.util.Map;

//import net.floodlightcontroller.core.FloodlightContext;
//import net.floodlightcontroller.core.IOFMessageListener;
//import net.floodlightcontroller.core.IOFSwitch;
import net.floodlightcontroller.core.IOFSwitchListener;
//import net.floodlightcontroller.core.PortChangeType;
//import net.floodlightcontroller.core.internal.FloodlightProvider;
import net.floodlightcontroller.core.internal.IOFSwitchService;
//import net.floodlightcontroller.core.module.FloodlightModuleContext;
//import net.floodlightcontroller.core.module.FloodlightModuleException;
//import net.floodlightcontroller.core.module.IFloodlightModule;
//import net.floodlightcontroller.core.module.IFloodlightService;
//import net.floodlightcontroller.storage.IStorageSourceService;
//import net.floodlightcontroller.threadpool.IThreadPoolService;

//import org.projectfloodlight.openflow.protocol.OFControllerRole;
//import org.projectfloodlight.openflow.protocol.OFMessage;
//import org.projectfloodlight.openflow.protocol.OFPortDesc;
//import org.projectfloodlight.openflow.protocol.OFRoleReply;
//import org.projectfloodlight.openflow.protocol.OFType;
import org.projectfloodlight.openflow.types.DatapathId;
import org.sdnplatform.sync.IStoreClient;
//import org.sdnplatform.sync.IStoreListener;
//import org.sdnplatform.sync.ISyncService;
//import org.sdnplatform.sync.ISyncService.Scope;
//import org.sdnplatform.sync.error.SyncException;
//import org.sdnplatform.sync.internal.rpc.IRPCListener;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;

public class FT implements
//        IOFMessageListener,
//        IFloodlightModule,
//        IStoreListener<String>,
        IOFSwitchListener
//        IRPCListener
{

//    private ISyncService syncService;
//    private IStoreClient<String, String> storeFT;
    public IStoreClient<String, String> storeFT;


//    protected static Logger logger = LoggerFactory.getLogger(FT.class);

    protected static IOFSwitchService switchService;
//    private static UtilDurable utilDurable;

    private String controllerId;

    /*
    @Override
    public String getName() {
        // TODO Auto-generated method stub
        return FT.class.getSimpleName();
    }

    @Override
    public boolean isCallbackOrderingPrereq(OFType type, String name) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isCallbackOrderingPostreq(OFType type, String name) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Collection<Class<? extends IFloodlightService>> getModuleServices() {
        // TODO Auto-generated method stub
        return null;

    }

    @Override
    public Map<Class<? extends IFloodlightService>, IFloodlightService> getServiceImpls() {
        // TODO Auto-generated method stub
        return null;

    }

    @Override
    public Collection<Class<? extends IFloodlightService>> getModuleDependencies() {
        // TODO Auto-generated method stub
        Collection<Class<? extends IFloodlightService>> l =
                new ArrayList<Class<? extends IFloodlightService>>();
        l.add(IStorageSourceService.class);
        l.add(ISyncService.class);
        l.add(IOFSwitchService.class);
        return l;
    }

    @Override
    public void init(FloodlightModuleContext context)
            throws FloodlightModuleException {
        // TODO Auto-generated method stub

        this.syncService = context.getServiceImpl(ISyncService.class);
        switchService = context.getServiceImpl(IOFSwitchService.class);
        utilDurable = new UtilDurable();

        Map<String, String> configParams = context.getConfigParams(FloodlightProvider.class);
        controllerId = configParams.get("controllerId");
    }

    @Override
    public void startUp(FloodlightModuleContext context)
            throws FloodlightModuleException {

        switchService.addOFSwitchListener(this);
        syncService.addRPCListener(this);

        try {
            this.syncService.registerStore("FT_Switches", Scope.GLOBAL);
            this.storeFT = this.syncService
                    .getStoreClient("FT_Switches",
                            String.class,
                            String.class);
            this.storeFT.addStoreListener(this);
        } catch (Exception e) {
            throw new FloodlightModuleException("Error while setting up sync service", e);
        }
    }

    @Override
    public net.floodlightcontroller.core.IListener.Command receive(
            IOFSwitch sw, OFMessage msg, FloodlightContext cntx) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void keysModified(Iterator<String> keys,
                             org.sdnplatform.sync.IStoreListener.UpdateType type) {
        // TODO Auto-generated method stub
        while(keys.hasNext()){
            String k = keys.next();
            try {
				/*logger.debug("keysModified: Key:{}, Value:{}, Type: {}",
						new Object[] {
							k,
							storeClient.get(k).getValue().toString(),
							type.name()}
						);
                if(type.name().equals("REMOTE")){
                    String swIds = storeFT.get(k).getValue();
                    logger.debug("REMOTE: NodeId:{}, Switches:{}", k, swIds);
                }
            } catch (SyncException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        }
    }
    */

    @Override
    public void switchAdded(DatapathId switchId) {
        // TODO Auto-generated method stub
    }

    @Override
    public void switchRemoved(DatapathId switchId) {
        // TODO Auto-generated method stub
        try {
            this.storeFT.put(controllerId, getActiveSwitches());
//        } catch (SyncException e) {
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    public void switchActivated(DatapathId switchId) {
        // TODO Auto-generated method stub
        String switches = getActiveSwitches();
        // expected addition:
        // if (switches == null) return;
        try {
            this.storeFT.put(controllerId, switches);
//        } catch (SyncException e) {
         } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /*
    @Override
    public void switchPortChanged(DatapathId switchId, OFPortDesc port,
                                  PortChangeType type) {
        // TODO Auto-generated method stub

    }
    */

    @Override
    public void switchChanged(DatapathId switchId) {
        // TODO Auto-generated method stub
        String switches = getActiveSwitches();
        if(switches==null)return;
        try {
            this.storeFT.put(controllerId, switches);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }


    public String getActiveSwitches(){
        if(switchService == null)return null;
        String activeSwitches = "";
        Iterator<DatapathId> itDpid = switchService.getAllSwitchDpids().iterator();
        while (itDpid.hasNext()) {
            DatapathId dpid = itDpid.next();
            try{
                if(switchService.getActiveSwitch(dpid).isActive()){
                    activeSwitches += dpid;
                    if(itDpid.hasNext())
                        activeSwitches += ",";
                }
            }
            catch(NullPointerException npe){
                return null;
            }
        }
        return activeSwitches;
    }

    /*
    public void setSwitchRole(OFControllerRole role, String swId){

        IOFSwitch sw = switchService.getActiveSwitch(DatapathId.of(swId));
        OFRoleReply reply=null;
        UtilDurable utilDurable = new UtilDurable();
        reply = utilDurable.setSwitchRole(sw, role);

        if(reply!=null){
            logger.info("DEFINED {} as {}, reply.getRole:{}!",
                    new Object[]{
                            sw.getId(),
                            role,
                            reply.getRole()});
        }
        else
            logger.info("Reply NULL!");
    }

    @Override
    public void disconnectedNode(Short nodeId) {
        // TODO Auto-generated method stub
        String swIds=null;
        try {
            swIds = storeFT.get(""+nodeId).getValue();
            logger.debug("Switches managed by nodeId:{}, Switches:{}", nodeId, swIds);
        } catch (SyncException e) {
            e.printStackTrace();
        }

        if(!swIds.equals("")){
            String swId[] = swIds.split(",");
            for (int i = 0; i < swId.length; i++) {
                setSwitchRole(OFControllerRole.ROLE_MASTER, swId[i]);
            }
        }
    }

    @Override
    public void connectedNode(Short nodeId) {
        // TODO Auto-generated method stub
        String activeSwicthes = getActiveSwitches();
        logger.debug("NodeID: {} connected, my switches: {}", nodeId, activeSwicthes);
        try {
            storeFT.put(controllerId, activeSwicthes);
        } catch (SyncException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    */
}