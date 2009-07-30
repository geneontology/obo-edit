package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class TrivialLink implements Link {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TrivialLink.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 2103978770082870493L;
	protected LinkedObject child;

	public TrivialLink(LinkedObject child) {
		this.child = child;
	}
	
	public String getID() {
		return child.getID()+" -null-> null";
	}
	
	public boolean isImplied() {
		return false;
	}

	public LinkedObject getParent() {
		return null;
	}

	public Namespace getNamespace() {
		return null;
	}

	public LinkedObject getChild() {
		return child;
	}

	public OBOProperty getType() {
		return null;
	}

	public NestedValue getNestedValue() {
		return null;
	}

	@Override
	public Object clone() {
		return new TrivialLink(child);
	}
		
	public boolean isAnonymous() {
		return false;
	}

	public void setType(OBOProperty property) {
		throw new UnsupportedOperationException("TrivialLinks are read-only");
	}

	public void setChild(LinkedObject child) {
		throw new UnsupportedOperationException("TrivialLinks are read-only");
	}

	public void setParent(LinkedObject parent) {
		throw new UnsupportedOperationException("TrivialLinks are read-only");
	}

	public void setNamespace(Namespace namespace) {
		throw new UnsupportedOperationException("TrivialLinks are read-only");
	}

	public void setNestedValue(NestedValue nv) {
		throw new UnsupportedOperationException("TrivialLinks are read-only");
	}
}
