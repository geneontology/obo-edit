package org.obo.nlp;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.nlp.impl.LogicalDefinitionNamer;
import org.obo.util.TermUtil;

public class NamerUtil {
	
	public static Namer getDefaultNamer() {
		return new LogicalDefinitionNamer();
	}
	public static void nameUnnamedObjects(OBOSession session) {
		nameUnnamedObjects(session, getDefaultNamer());
	}
	public static void nameUnnamedObjects(OBOSession session, Namer namer) {
		for (OBOObject io : TermUtil.getOBOObjects(session)) {
			if (io.getName() == null) {
				Collection<String> names = namer.constructNames(io);
				io.setName(names.iterator().next());
			}
			if (io.getName() == null) {
				io.setName(io.getID());
			}
		}
	}
	public static Collection<HistoryItem> getNameUnnamedObjectsAction(OBOSession session) {
		return getNameUnnamedObjectsAction(session, getDefaultNamer());
	}
	public static Collection<HistoryItem> getNameUnnamedObjectsAction(OBOSession session, Namer namer) {
		Collection<HistoryItem> items = new LinkedList<HistoryItem>();
		for (OBOObject io : TermUtil.getOBOObjects(session)) {
			String newName = null;
			if (io.getName() == null) {
				Collection<String> names = namer.constructNames(io);
				for (String name : names) {
					newName = name;
					break;
				}
				if (newName == null) {
					newName = io.getID();
				}
			}
			if (newName != null) {
				items.add(new NameChangeHistoryItem(io,newName));
			}
		}
		return items;
	}

}
