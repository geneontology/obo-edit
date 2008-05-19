package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class NameSynonymSearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameSynonymSearchCriterion.class);

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof IdentifiedObject) {
			String name = ((IdentifiedObject) obj).getName();
			if (name != null)
				scratch.add(name);
		}
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
