package org.obo.datamodel;

import java.util.Set;
import java.io.Serializable;

public interface NestedValue extends Cloneable, Serializable {

	public String getName();

	public Set<PropertyValue> getPropertyValues();

	public void addPropertyValue(PropertyValue pv);

	public Object clone();
	
	public String getSuggestedComment();
	
	public void setSuggestedComment(String comment);
}
