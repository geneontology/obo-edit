package org.obo.util;

import java.util.logging.Logger;

import org.obo.datamodel.OBOSession;

public class SessionWrapper {
	protected Logger logger = Logger.getLogger("org.obo.util");
	
	protected OBOSession session;

	public OBOSession getSession() {
		return session;
	}

	public void setSession(OBOSession session) {
		this.session = session;
	}
	
	

}
