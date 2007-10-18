package org.obo.datamodel.impl;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOProperty;

public class DanglingLinkImpl implements Link {
	
	protected LinkedObject child;
	protected LinkedObject parent;
	protected OBOProperty type;
	
	public DanglingLinkImpl(String childID, String typeID, String parentID) {
		this.child = new DanglingObjectImpl(childID);
		this.parent = new DanglingObjectImpl(parentID);
		this.type = new DanglingPropertyImpl(typeID);
	}
	
	public boolean isAnonymous() {
		return false;
	}
	
	public boolean isImplied() {
		return false;
	}

	public LinkedObject getParent() {
		return parent;
	}

	public LinkedObject getChild() {
		return child;
	}
	
	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			return new DanglingLinkImpl(child.getID(), type.getID(), parent.getID());
		}
	}
	
	public String toString() {
		return "..."+child+" -"+type+"-> "+parent+"...";
	}

	public OBOProperty getType() {
		return type;
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

	public void setNamespace(Namespace namespace) {
		throw new UnsupportedOperationException();
	}

	public void setParent(LinkedObject parent) {
		throw new UnsupportedOperationException();
	}
	
	public String getID() {
		char sepChar = '-';
		return getChild().getID()+' '+sepChar+getType().getID()+sepChar+'>'+ ' '+getParent().getID();
	}
}
