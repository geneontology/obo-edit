package org.obo.datamodel;

import java.util.Set;

public interface DbxrefedObject {
	public Set<Dbxref> getDbxrefs();

	public void addDbxref(Dbxref xref);

	public void removeDbxref(Dbxref xref);
}
