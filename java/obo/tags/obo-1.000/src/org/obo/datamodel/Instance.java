package org.obo.datamodel;

import java.util.Collection;

public interface Instance extends AnnotatedObject,
		LinkedObject {

	public void addPropertyValue(OBOProperty property, Value<?> value);

	public void removePropertyValue(OBOProperty property, Value<?> value);

	public Collection<Value<?>> getValues(OBOProperty property);

	public Collection<OBOProperty> getProperties();

	public void setType(OBOClass type);
}
