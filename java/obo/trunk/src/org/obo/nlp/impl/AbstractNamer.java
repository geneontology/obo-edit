package org.obo.nlp.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.SynonymedObject;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.Namer;
import org.obo.util.ReasonerUtil;

public abstract class AbstractNamer implements Namer {

	public Collection<String> constructNames(LinkedObject lo) {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<String> constructTextDefs(LinkedObject lo) {
		// TODO Auto-generated method stub
		return null;
	}

	public Collection<AddSynonymHistoryItem> generateSynonymChanges(OBOSession session) {
		 Collection<AddSynonymHistoryItem> items =
			 new ArrayList<AddSynonymHistoryItem>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (!(io instanceof SynonymedObject))
				continue;
			Collection<String> names = constructNames((LinkedObject)io);
			for (String name : names) {
				 AddSynonymHistoryItem item = 
					 new AddSynonymHistoryItem((SynonymedObject)io, name);
				items.add(item);
			}
		}
		return items;
	}
	
	public Collection<DefinitionChangeHistoryItem> generateDefinitionChanges(OBOSession session) {
		 Collection<DefinitionChangeHistoryItem> items =
			 new ArrayList<DefinitionChangeHistoryItem>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (!(io instanceof DefinedObject))
				continue;
			if (((DefinedObject)io).getDefinition() == null)
				continue;
			Collection<String> defs = constructTextDefs((LinkedObject)io);
			for (String def : defs) {
				DefinitionChangeHistoryItem item = 
					 new DefinitionChangeHistoryItem((DefinedObject)io, def);
				items.add(item);
			}
		}
		return items;
		
	}

	
	protected boolean isSubclassByName(LinkedObject lo, String superName) {
		if (lo.getName().equals(superName))
			return true;
		for (Link tr : lo.getParents()) {
			if (ReasonerUtil.isSubclass(tr.getType(), OBOProperty.IS_A)
					&& isSubclassByName(tr.getParent(), superName))
				return true;
		}
		return false;
	}


}
