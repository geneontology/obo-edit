package org.obd.ws.restServlet;

import org.restlet.Component;
import org.restlet.data.Protocol;


public class OBDRestComponent extends Component{
	
	public OBDRestComponent(){
		super();
		this.getClients().add(Protocol.FILE);
		/*
		System.out.println("LOGGER: " + this.getLogService().getLoggerName());
		Logger.getLogger(this.getLogService().getLoggerName()).setLevel(Level.FINEST);
		System.out.println("LEVEL:" + Logger.getLogger((this.getLogService().getLoggerName())).getLevel());
		*/
	}
	
}