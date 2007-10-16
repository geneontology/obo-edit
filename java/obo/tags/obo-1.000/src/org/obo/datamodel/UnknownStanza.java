package org.obo.datamodel;

import java.util.LinkedList;
import java.util.List;

public class UnknownStanza {

	protected String stanza;
	protected Namespace namespace;
	protected List<PropertyValue> propertyValues;

	public UnknownStanza(String stanza, Namespace ns) {
		this.stanza = stanza;
		this.namespace = ns;
		this.propertyValues = new LinkedList<PropertyValue>();
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

	public void addPropertyValue(PropertyValue pv) {
		propertyValues.add(pv);
	}
}
