package org.obo.datamodel.impl;

import org.obo.datamodel.*;

public class OBOClassImpl extends LinkedAnnotatedObjectImpl implements OBOClass {

	/**
	 * 
	 */
	private static final long serialVersionUID = -9157839845696719063L;

	protected boolean isRoot = false;

	public OBOClassImpl(String id) {
	    // Hey, this sets the name to the id!  I thought we didn't want that.  --NH, 4/2008
	    // this(id, id);
//	    this(null, id);
	    // I'm putting this back the way it was, because making the name null broke cloning.
	    // If you want to assign a different name to the object, you can
	    // use the method below that takes a term name and an id.
	    this(id, id);
	}

	public OBOClassImpl(String term, String id) {
		super(id);
		setName(term);
	}

	public Type<OBOClass> getType() {
		return OBO_CLASS;
	}

	@Override
	public void setIsAnonymous(boolean anonymous) {
		this.anonymous = anonymous;
	}

	public boolean isType() {
		return false;
	}

	@Override
	public String toString() {
		return getName();
	}

	public void setID(String id) {
		this.id = id;
	}

	public int compareTo(Object in) {
		if (in instanceof OBOClass) {
			int cmp = getName().toUpperCase().compareTo(
					((OBOClass) in).getName().toUpperCase());
			if (cmp == 0)
				return getID().toUpperCase().compareTo(
						((OBOClass) in).getID().toUpperCase());
			else
				return cmp;
		} else {
			return toString().compareToIgnoreCase(in.toString());
		}
	}
}
