package org.obo.datamodel;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.*;

public class UnknownStanza {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UnknownStanza.class);

	protected String stanza;
	protected Namespace namespace;
	protected List<PropertyValue> propertyValues;
	protected List<NestedValue> nestedValues;

	public UnknownStanza(String stanza, Namespace ns) {
		this.stanza = stanza;
		this.namespace = ns;
		this.propertyValues = new ArrayList<PropertyValue>();
		this.nestedValues = new ArrayList<NestedValue>();
	}

	public Namespace getNamespace() {
		return namespace;
	}

	public String getStanza() {
		return stanza;
	}

	public List<PropertyValue> getPropertyValues() {
		return propertyValues;
	}

	public void addPropertyValue(PropertyValue pv, NestedValue nv) {
		propertyValues.add(pv);
		nestedValues.add(nv);
	}
	
	public NestedValue getNestedValue(PropertyValue pv) {
		int index = propertyValues.indexOf(pv);
		if (index >= 0 && index < nestedValues.size())
			return nestedValues.get(index);
		else
			return null;
	}
}
