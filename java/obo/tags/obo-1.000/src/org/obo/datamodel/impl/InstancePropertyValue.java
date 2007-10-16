package org.obo.datamodel.impl;

import org.bbop.util.*;
import org.obo.datamodel.*;

public class InstancePropertyValue implements ValueLink {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6681655687513873674L;
	protected LinkedObject child;
	protected OBOProperty type;
	protected Value value;
	protected NestedValue nv;
	protected Namespace namespace;
	protected boolean isImplied = false;

	public InstancePropertyValue() {
	}

	public InstancePropertyValue(LinkedObject child) {
		this(child, null, null);
	}
	
	public InstancePropertyValue(LinkedObject child, OBOProperty type,
			Value value) {
		this(child, type, value, false);
	}

	public InstancePropertyValue(LinkedObject child, OBOProperty type,
			Value value, boolean isImplied) {
		this.child = child;
		this.type = type;
		this.value = value;
		this.isImplied = isImplied;
	}
	
	public void setNamespace(Namespace namespace) {
		this.namespace = namespace;
	}
	
	public Namespace getNamespace() {
		return namespace;
	}

	@Override
	public int hashCode() {
		return (child == null ? 0 : child.hashCode())
				+ (type == null ? 0 : type.hashCode())
				+ (value == null ? 0 : value.hashCode());
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof InstancePropertyValue) {
			InstancePropertyValue ipv = (InstancePropertyValue) o;
			return ObjectUtil.equals(ipv.getChild(), child)
					&& ObjectUtil.equals(ipv.getType(), type)
					&& ObjectUtil.equals(ipv.getValue(), value);
		} else
			return false;
	}

	@Override
	public String toString() {
		return child + " --" + (type == null ? "null type" : type.getID())
				+ "--> " + value;
	}

	public LinkedObject getChild() {
		return child;
	}

	public void setChild(LinkedObject child) {
		this.child = child;
	}

	public OBOProperty getType() {
		return type;
	}

	public void setType(OBOProperty type) {
		this.type = type;
	}

	public void setNestedValue(NestedValue nv) {
		this.nv = nv;
	}

	public NestedValue getNestedValue() {
		return nv;
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			return null;
		}
	}

	public Value getValue() {
		return value;
	}

	public void setValue(Value value) {
		this.value = value;
	}
	
	public LinkedObject getParent() {
		if (value instanceof LinkedObject)
			return (LinkedObject) value;
		else
			return null;
	}
	
	public void setParent(LinkedObject parent) {
		setValue(parent);
	}
	
	public String getID() {
		char sepChar = '-';
		String strVal = null;
		if (value instanceof DatatypeValue)
			strVal = ((DatatypeValue) value).getValue();
		else
			strVal = value.toString();
		return getChild().getID()+' '+sepChar+getType().getID()+sepChar+'>'+ " \""+strVal.replaceAll("\"", "\\\"")+"\"";
	}
	
	public boolean isAnonymous() {
		return false;
	}
	
	public boolean isImplied() {
		return isImplied;
	}
	
}
