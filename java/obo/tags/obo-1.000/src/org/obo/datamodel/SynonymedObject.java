package org.obo.datamodel;

import java.util.Set;

public interface SynonymedObject extends IdentifiedObject {

	public Set<Synonym> getSynonyms();

	public void addSynonym(Synonym s);

	public void removeSynonym(Synonym s);
}
