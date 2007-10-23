package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class NameSynonymSearchCriterion extends AbstractStringCriterion {

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof IdentifiedObject)
			scratch.add(((IdentifiedObject) obj).getName());
		if (obj instanceof SynonymedObject) {
			Iterator it = ((SynonymedObject) obj).getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				scratch.add(s.getText());
			}
		}
		return scratch;
	}

	public String getID() {
		return "name_or_synonym";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Name or Synonym";
	}
}
