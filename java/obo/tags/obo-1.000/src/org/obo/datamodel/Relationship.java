package org.obo.datamodel;

import java.io.Serializable;

public interface Relationship extends Serializable, Cloneable {

	public LinkedObject getChild();

	public void setChild(LinkedObject child);

	public OBOProperty getType();

	public void setType(OBOProperty type);

	public void setNestedValue(NestedValue nv);

	public NestedValue getNestedValue();

	public Object clone();
}
