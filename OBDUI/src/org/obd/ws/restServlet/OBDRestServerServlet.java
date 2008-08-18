package org.obd.ws.restServlet;

import org.restlet.Application;
import org.restlet.Context;
import com.noelios.restlet.ext.servlet.ServerServlet;

public class OBDRestServerServlet extends ServerServlet {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1991348292180577525L;

	public OBDRestServerServlet(){
		super();
	}
	
	public String getRealPath(String path){
		return this.getServletContext().getRealPath(path);
	}
	
	public Application createApplication(Context context){
		OBDRestApplication a = (OBDRestApplication) super.createApplication(context);
		a.setServerServlet(this);
		return a;
	}
	
}