package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class SynonymSearchCriterion extends AbstractCriterion<IdentifiedObject, Synonym> {

	public static final SynonymSearchCriterion CRITERION = new SynonymSearchCriterion();
	
	public SynonymSearchCriterion() {
	}	
	
	public Collection<Synonym> getValues(Collection<Synonym> scratch,
			IdentifiedObject obj) {
		if (obj instanceof SynonymedObject) {
			Iterator it = ((SynonymedObject) obj).getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				scratch.add(s);
			}
		}
		return scratch;
	}
	
	public String getID() {
		return "synonym";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Synonym";
	}

	public Class<Synonym> getReturnType() {
		return Synonym.class;
	}
}
