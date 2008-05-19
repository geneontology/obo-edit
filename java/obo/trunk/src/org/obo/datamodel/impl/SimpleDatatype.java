package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class SimpleDatatype<T> extends AnnotatedObjectImpl implements Datatype<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -7226239445473407504L;

	public SimpleDatatype() {
		super("xsd:simpleType");
	}

	public Type getType() {
		return OBOClass.OBO_DATATYPE;
	}

	@Override
	public boolean isBuiltIn() {
		return true;
	}

	@Override
	public boolean isAnonymous() {
		return false;
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public boolean isObsolete() {
		return false;
	}

	@Override
	public Set getSecondaryIDs() {
		return Collections.EMPTY_SET;
	}

	@Override
	public String getComment() {
		return "The default supertype of all primitive datatypes";
	}

	@Override
	public Namespace getNamespace() {
		return null;
	}

	public boolean isAbstract() {
		return true;
	}

	public Datatype<Object> getSupertype() {
		return null;
	}

	public boolean isLegalValue(String string) {
		return false;
	}



	@Override
	public String toString() {
		return getID();
	}

	public String getString(T o) {
		return null;
	}

	public T getValue(String string) {
		return null;
	}
}
