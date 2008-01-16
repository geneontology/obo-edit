package org.obo.nlp;

import java.util.Collection;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;

public interface SemanticParser {

	public Collection<String> getReports();
	public void index(OBOSession session);
	public Collection<TermMacroHistoryItem> parseTerms();

	Collection<TermMacroHistoryItem> parseTerm(LinkedObject lo);
	public void apply(Collection<TermMacroHistoryItem> items);
	
}
