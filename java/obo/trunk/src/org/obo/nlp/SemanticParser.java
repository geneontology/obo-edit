package org.obo.nlp;

import java.util.Collection;

import org.obo.datamodel.LinkedObject;
import org.obo.history.TermMacroHistoryItem;

public interface SemanticParser {

	Collection<TermMacroHistoryItem> parseTerm(LinkedObject lo);
	
}
