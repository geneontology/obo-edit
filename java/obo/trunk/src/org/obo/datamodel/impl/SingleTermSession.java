package org.obo.datamodel.impl;

import org.obo.datamodel.CategorizedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;

import org.apache.log4j.*;

public class SingleTermSession extends OBOSessionImpl {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SingleTermSession.class);

	public SingleTermSession(OBOSession basis, IdentifiedObject term) {
		if (basis != null) {
			categories.addAll(basis.getCategories());
			synonymCategories.addAll(basis.getSynonymCategories());
			namespaces.addAll(basis.getNamespaces());
		} else {
			if (term instanceof CategorizedObject) {
				categories.addAll(((CategorizedObject) term).getCategories());
			}
			if (term instanceof SynonymedObject) {
				for (Synonym s : ((SynonymedObject) term).getSynonyms()) {
					if (s.getSynonymCategory() != null) {
						synonymCategories.add(s.getSynonymCategory());
					}
				}
			}
			if (term.getNamespace() != null)
				namespaces.add(term.getNamespace());
		}
		addObject(term);
	}
}
