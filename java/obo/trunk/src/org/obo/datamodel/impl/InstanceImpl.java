package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class InstanceImpl extends LinkedAnnotatedObjectImpl implements
	Instance {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(InstanceImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -9207122228671403312L;

	protected OBOClass type;

	protected NestedValue nv;

	public InstanceImpl(String id, OBOClass type) {
		super(id);
		setType(type);
		// CJM 2008-05-07 : leave name as null if not set
		//setName(id);
	}

	public InstanceImpl(String id) {
		this(id, null);
	}

	@Override
	public String toString() {
		return name + " (" + id + ")";
	}

	public InstanceImpl(OBOClass type) {
		this(null, type);
	}

	public void setTypeNestedValue(NestedValue nv) {
		this.nv = nv;
	}

	public NestedValue getTypeNestedValue() {
		return nv;
	}

	public void setType(OBOClass type) {
		this.type = type;
	}

	public Type<OBOClass> getType() {
		return type;
	}

	public void addPropertyValue(OBOProperty property, Value<?> value) {
		InstancePropertyValue ipv = new InstancePropertyValue(this, property,
				value);

		addParent(ipv);
	}

	public void removePropertyValue(OBOProperty property, Value<?> value) {
		for (Link link : getParents()) {
			if (link.getType().equals(property)) {
				if (link instanceof ValueLink) {
					InstancePropertyValue ipv = (InstancePropertyValue) link;
					if (ipv.getValue().equals(value)) {
						removeParent(ipv);
						break;
					}
				} else {
					removeParent(link);
					break;
				}
			}
		}
	}

	public Collection<Value<?>> getValues(OBOProperty property) {
		Set<Value<?>> values = new LinkedHashSet<Value<?>>();
		for (Link l : getParents()) {
			if (l.getType() != null && l.getType().equals(property)) {
				if (l instanceof ValueLink) {
					ValueLink link = (ValueLink) l;
					if (link.getType() != null
							&& link.getType().equals(property))
						values.add(link.getValue());
				} else
					values.add(l.getParent());
			}
		}
		return values;
	}

	public Collection<OBOProperty> getProperties() {
		Set<OBOProperty> properties = new LinkedHashSet<OBOProperty>();
		for (Link link: getParents()) {
			if (link instanceof ValueLink)
				properties.add(((ValueLink)link).getType());
		}
		return properties;
	}
}
