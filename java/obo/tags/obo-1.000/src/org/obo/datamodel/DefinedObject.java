package org.obo.datamodel;

import java.util.Set;

public interface DefinedObject extends IdentifiedObject {

	public String getDefinition();

	public void setDefinition(String definition);

	public Set<Dbxref> getDefDbxrefs();

	public void addDefDbxref(Dbxref dbxref);

	public void removeDefDbxref(Dbxref dbxref);

	public void setDefinitionExtension(NestedValue nv);

	public NestedValue getDefinitionExtension();
}
