package org.oboedit.gui.actions;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.impl.DanglingObjectImpl;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.NamespaceHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.IDUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.util.GUIUtil;

public class TypedAddAction extends AddAction {
	protected boolean isType;

	public TypedAddAction(boolean isType) {
		this.isType = isType;
	}

	@Override
	public void clickInit(Selection sourceItems, GestureTarget destItem) {
	}

	@Override
	public String getName() {
		if (isType)
			return "Add type root";
		else
			return "Add root";
	}

	@Override
	public String getDesc() {
		return null;
	}

	@Override
	public boolean isLegal() {
		return true;
	}

	@Override
	public HistoryItem execute() {
		String id = null;
		if (isType)
			id = getTypeID();
		else
			id = GUIUtil.fetchID(null);
		if (id == null)
			return null;
		/*
		 * OBOClass newTerm = (OBOClass)
		 * controller.getHistory().getObjectFactory(). createObject(id, (isType ?
		 * OBOClass.OBO_PROPERTY : OBOClass.OBO_CLASS));
		 */
		TermMacroHistoryItem item = new TermMacroHistoryItem("Created new "
				+ (isType ? "type " : "") + "root");
		String typeID = (isType ? OBOClass.OBO_PROPERTY.getID()
				: OBOClass.OBO_CLASS.getID());
		item.addItem(new CreateObjectHistoryItem(id, typeID));
		item.addItem(new NameChangeHistoryItem("<new term>", id, id));

		Namespace ns = SessionManager.getManager().getSession()
				.getDefaultNamespace();
		if (ns != null)
			item.addItem(new NamespaceHistoryItem(null, ns, id));

		if (Preferences.getPreferences().getUsePersonalDefinition()) {
			item.addItem(new DefinitionChangeHistoryItem(null, Preferences
					.getPreferences().getPersonalDefinition(), id));
			Iterator it = Preferences.getPreferences().getPersonalDbxrefs()
					.iterator();
			while (it.hasNext()) {
				Dbxref ref = (Dbxref) it.next();
				item.addItem(new AddDbxrefHistoryItem(id, ref, true, null));
			}
		}
		Collection<LinkedObject> wrapper = new LinkedList<LinkedObject>();
		wrapper.add(new DanglingObjectImpl(id));
		GUIUtil.setPostSelection(item, SelectionManager
				.createSelectionFromTerms(null, wrapper, null, false));
		return item;
	}

}
