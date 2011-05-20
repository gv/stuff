package vg.Sc;

import java.lang.Thread;

import java.io.IOException;
import org.teleal.cling.UpnpService;
import org.teleal.cling.UpnpServiceImpl;
import org.teleal.cling.binding.*;
import org.teleal.cling.binding.annotations.*;
import org.teleal.cling.model.*;
import org.teleal.cling.model.meta.*;
import org.teleal.cling.model.types.*;
import org.teleal.cling.registry.*;
import org.teleal.cling.model.message.header.*;

//import org.teleal.cling.android.AndroidUpnpServiceConfiguration;

public class NetworkNode extends DefaultRegistryListener {
	/*
		API
	*/
		
	public class Status {
		public String name;
		public Status(Device dev) {
			name = dev.getDisplayString();
		}
		public boolean equals(Status other) {
			return other.name == name;
		}
	}

	public interface User {
		void handleOtherNodePresence(Status s);
		void handleOtherNodeQuit(Status s);
	}

	public NetworkNode(User u) 
		throws ValidationException, LocalServiceBindingException, IOException {
		m_user = u;
		m_upnp = new UpnpServiceImpl();
		init();
	}

	public NetworkNode(User u, UpnpService upnp) 
		throws ValidationException, LocalServiceBindingException, IOException {
		m_user = u;
		m_upnp = upnp;
		init();
		for (Device device : m_upnp.getRegistry().getDevices()) {
			//deviceAddedHlp(m_upnp.getRegistry(), device);
		}
	}		
	
	private void init() 
		throws ValidationException, LocalServiceBindingException, IOException {
		m_upnpDev = createDevice();
		m_upnp.getRegistry().addDevice(m_upnpDev);
		m_upnp.getRegistry().addListener(this);
		refresh();
	}

	public void refresh() {
		// Broadcast a search message for all devices
		m_upnp.getControlPoint().search(new STAllHeader());
	}		

	public void quit() throws InterruptedException {
		m_upnp.shutdown();
	}
	
	public String report() {
		String r = "";
		for(NetworkAddress na: m_upnp.getRouter().getActiveStreamServers(null)) {
			r += na.getAddress().toString();
		}
		return r;
	}
				

	private LocalDevice m_upnpDev;
	private UpnpService m_upnp;
	private User m_user;
 
	/*
		RegistryListener
	*/

	ServiceId serviceId = new UDAServiceId("DuctTapedCameras");

	@Override
		public void remoteDeviceAdded(Registry registry, RemoteDevice device) {
		deviceAddedHlp(registry, device);
	}

	private void deviceAddedHlp(Registry registry, Device device) {
		Service switchPower;
		if ((switchPower = device.findService(serviceId)) != null) {
			Status s = new Status(device);
			m_user.handleOtherNodePresence(s);
			//executeAction(upnpService, switchPower);
		}
	}

	@Override
		public void remoteDeviceRemoved(Registry registry, RemoteDevice device) {
		Service switchPower;
		if ((switchPower = device.findService(serviceId)) != null) {
			Status s = new Status(device);
			m_user.handleOtherNodeQuit(s);
		}
	}
	

	LocalDevice createDevice()
		throws ValidationException, LocalServiceBindingException, IOException {
    DeviceIdentity identity =
			new DeviceIdentity(UDN.uniqueSystemIdentifier("Duct taped cameras"));
    DeviceType type = new UDADeviceType("DuctTapedCameras", 1);
    DeviceDetails details = new DeviceDetails("A synchronizable camera",
			new ManufacturerDetails("vg"),
			new ModelDetails(
				"Hi",
				"what",
				"v1"
			)
		);

    /*Icon icon =  new Icon("image/png", 48, 48, 8,
                    getClass().getResource("icon.png"));*/

    LocalService<SwitchPower> switchPowerService =
            new AnnotationLocalServiceBinder().read(SwitchPower.class);
    switchPowerService.setManager(
            new DefaultServiceManager(switchPowerService, SwitchPower.class)
    );
    return new LocalDevice(identity, type, details, /*icon,*/ switchPowerService);
	}

}
	


@org.teleal.cling.binding.annotations.UpnpService(
        serviceId = @UpnpServiceId("DuctTapedCameras"),
        serviceType = @UpnpServiceType(value = "DuctTapedCameras", version = 1)
)
class SwitchPower {

    @UpnpStateVariable(defaultValue = "0", sendEvents = false)
    private boolean target = false;

    @UpnpStateVariable(defaultValue = "0")
    private boolean status = false;

    @UpnpAction
    public void setTarget(@UpnpInputArgument(name = "NewTargetValue")
                          boolean newTargetValue) {
        target = newTargetValue;
        status = newTargetValue;
        System.out.println("Switch is: " + status);
    }

    @UpnpAction(out = @UpnpOutputArgument(name = "RetTargetValue"))
    public boolean getTarget() {
        return target;
    }

    @UpnpAction(out = @UpnpOutputArgument(name = "ResultStatus"))
    public boolean getStatus() {
        return status;
    }

}




/*
import java.net.MulticastSocket;
import java.net.InetAddress;

public class NetworkNode {
	public class Status {
		public String name;
	}

	public interface User {
		void handleOtherNodePresence(Status s);
	}

	private static final String MSG_HEAD = "Duct taped cameras";
	private static final String MSG_WHO_S_HERE = "hey";
	private static final String MSG_I_M_HERE = "what";

	private static final int PORT_NUMBER = 64172;

	private MulticastSocket m_sock;
	private InetAddress m_group;
	private Thread m_serverThread;
	private bool m_over;
	private User m_user;

	public NetworkNode(User u) {
		m_over = false;
		m_group = InetAddress.getByName("228.5.6.7");
		m_sock = new MulticastSocket();
		m_sock.joinGroup(group);

		m_serverThread = new Thread() {
				int run() {
					byte[] buf = new byte[1000];
					DatagramPacket recv = new DatagramPacket(buf, buf.length);
					while(!m_over) {
						try {
							m_sock.receive(recv);
							
							

						} catch(java.net.SocketTimeoutException) {}
					}
					s.leaveGroup(group);
				}
			};
		m_serverThread.start();
	}

	public ask() {
		String req = MSG_HEAD + "," + MSG_WHO_S_HERE;
		DatagramPacket hi = new DatagramPacket(req.getBytes(), req.length(),
                             m_group, PORT_NUMBER);
		m_sock.send(hi);
 	}
}
*/
