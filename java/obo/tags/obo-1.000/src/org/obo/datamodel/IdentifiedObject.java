package org.obo.datamodel;

import java.io.Serializable;
import java.util.*;

public interface IdentifiedObject extends IdentifiableObject, NamespacedObject,
		Value, Cloneable, Serializable {

	public void setIsAnonymous(boolean isAnonymous);

	public Type<OBOClass> getType();

	public void setTypeExtension(NestedValue value);

	public NestedValue getTypeExtension();

	public Object clone();

	public boolean isBuiltIn();

	public String getName();

	public void setName(String name);

	public void setNameExtension(NestedValue nv);

	public NestedValue getNameExtension();

	public void setIDExtension(NestedValue nv);

	public void setAnonymousExtension(NestedValue nv);

	public NestedValue getIDExtension();

	public NestedValue getAnonymousExtension();

	public void addPropertyValue(PropertyValue pv);

	public Set<PropertyValue> getPropertyValues();

	public void removePropertyValue(PropertyValue pv);
}
