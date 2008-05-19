package org.obo.datamodel.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import org.obo.datamodel.Instance;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.TermCategory;
import org.obo.datamodel.Type;
import org.obo.datamodel.Value;

import org.apache.log4j.*;

public class DanglingInstanceImpl extends DanglingAnnotatedObjectImpl implements
	Instance {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DanglingInstanceImpl.class);

	protected OBOClass type = OBOClass.OBO_UNKNOWN;

	public DanglingInstanceImpl(String id, OBOClass type) {
		super(id);
		if (type != null)
			setType(type);
	}

	@Override
	public Type<OBOClass> getType() {
		return type;
	}

	public void addPropertyValue(OBOProperty property, Value<?> value) {
		throw new UnsupportedOperationException(
				"Cannot add property values to dangling instances");
	}

	public Collection<OBOProperty> getProperties() {
		return Collections.emptySet();
	}

	public Collection<Value<?>> getValues(OBOProperty property) {
		return Collections.emptySet();
	}

	public void removePropertyValue(OBOProperty property, Value<?> value) {
		throw new UnsupportedOperationException(
		"Cannot modify dangling instances");
	}

	public void setType(OBOClass type) {
		this.type = type;
	}
}
