package org.obo.util;

import org.apache.log4j.*;

import org.obo.datamodel.OBOSession;

import org.apache.log4j.*;

public class SessionWrapper {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SessionWrapper.class);
	
	protected OBOSession session;

	public OBOSession getSession() {
		return session;
	}

	public void setSession(OBOSession session) {
		this.session = session;
	}
	
	

}
