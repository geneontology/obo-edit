package org.obo.nlp;

import java.util.Collection;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.TermMacroHistoryItem;

public interface Namer {

	public Collection<AddSynonymHistoryItem> generateSynonymChanges(OBOSession session);

	public Collection<String> constructNames(LinkedObject lo);
	public Collection<String> constructTextDefs(LinkedObject lo);
	
}
