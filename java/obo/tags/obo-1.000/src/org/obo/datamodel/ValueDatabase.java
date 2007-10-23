package org.obo.datamodel;

import java.util.Collection;

public interface ValueDatabase extends IdentifiedObjectIndex {

	public Collection<IdentifiedObject> getObjects();
	public Collection<Value<?>> getValues(OBOProperty property, LinkedObject lo);
}
