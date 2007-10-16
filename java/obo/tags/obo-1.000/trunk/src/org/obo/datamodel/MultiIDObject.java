package org.obo.datamodel;

import java.util.Set;

public interface MultiIDObject extends IdentifiedObject {

	public void addSecondaryID(String secondaryID);

	public void removeSecondaryID(String secondaryID);

	public Set<String> getSecondaryIDs();

	public void addSecondaryIDExtension(String id, NestedValue nv);

	public NestedValue getSecondaryIDExtension(String id);
}
