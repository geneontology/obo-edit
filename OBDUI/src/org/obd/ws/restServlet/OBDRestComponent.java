package org.obd.ws.restServlet;

import org.restlet.Component;
import org.restlet.data.Protocol;


public class OBDRestComponent extends Component{
	
	public OBDRestComponent(){
		super();
		this.getClients().add(Protocol.FILE);
	}
	
}