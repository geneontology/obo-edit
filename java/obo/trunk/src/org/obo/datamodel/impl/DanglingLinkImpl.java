package org.obo.datamodel.impl;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOProperty;

import org.apache.log4j.*;

public class DanglingLinkImpl implements Link {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DanglingLinkImpl.class);
	
	protected String childID;
	protected String parentID;
	protected String typeID;
	
	public DanglingLinkImpl(String childID, String typeID, String parentID) {
		this.childID = childID;
		this.typeID = typeID;
		this.parentID = parentID;
	}
	
	public boolean isAnonymous() {
		return false;
	}
	
	public boolean isImplied() {
		return false;
	}

	public LinkedObject getParent() {
		return new DanglingObjectImpl(parentID);
	}

	public LinkedObject getChild() {
		return new DanglingObjectImpl(childID);
	}
	
	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			return new DanglingLinkImpl(childID, typeID, parentID);
		}
	}
	
	public String toString() {
		return "..."+getID()+"...";
	}

	public OBOProperty getType() {
		return new DanglingPropertyImpl(typeID);
	}

	public NestedValue getNestedValue() {
		return null;
	}

	public Namespace getNamespace() {
		return null;
	}

	public void setChild(LinkedObject child) {
		throw new UnsupportedOperationException();
	}

	public void setNestedValue(NestedValue nv) {
		throw new UnsupportedOperationException();
	}

	public void setType(OBOProperty type) {
		throw new UnsupportedOperationException();
	}
	
	public void setChild(String childID) {
		this.childID = childID;
	}
	
	public void setParent(String parentID) {
		this.parentID = parentID;
	}
	
	public void setType(String typeID) {
		this.typeID = typeID;
	}

	public void setNamespace(Namespace namespace) {
		throw new UnsupportedOperationException();
	}

	public void setParent(LinkedObject parent) {
		throw new UnsupportedOperationException();
	}
	
	public String getID() {
		char sepChar = '-';
		return childID+' '+sepChar+typeID+sepChar+'>'+ ' '+parentID;
	}
}
