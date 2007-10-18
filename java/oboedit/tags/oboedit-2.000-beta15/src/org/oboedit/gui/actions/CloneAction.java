package org.oboedit.gui.actions;


import java.awt.event.*;
import java.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.IDUtil;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;

public class CloneAction implements ClickMenuAction {

	protected Selection sources;

	protected boolean isLegal = false;

	protected static KeyStroke keyStroke = KeyStroke.getKeyStroke(
			KeyEvent.VK_L, InputEvent.CTRL_MASK);

	public KeyStroke getKeyStroke() {
		return keyStroke;
	}

	public String getName() {
		return "Clone";
	}

	public String getDesc() {
		return "Clone";
	}

	public List<EditAction> getSubActions() {
		return null;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = sources.getTerms().size() > 0;
		this.sources = sources;
	}

	public boolean isLegal() {
		return isLegal;
	}

	protected ObjectFactory defaultFactory = new DefaultObjectFactory();

	public HistoryItem execute() {
		List<HistoryItem> itemList = new LinkedList<HistoryItem>();
		Iterator it = sources.getTerms().iterator();
		Collection<String> ids = new HashSet<String>();
		List<LinkedObject> newTerms = new LinkedList<LinkedObject>();
		while (it.hasNext()) {
			HistoryList historyList = new DefaultHistoryList();
			IdentifiedObject io = (IdentifiedObject) it.next();
			io = (IdentifiedObject) io.clone();

			String id = IDUtil.fetchID(IDManager.getManager().getIDAdapter(),
					SessionManager.getManager().getSession(), null, ids, false);
			ids.add(id);

			io.setName("CLONE OF " + io.getName());

			IdentifiedObject clone = defaultFactory.createObject(id,
					(OBOClass) io.getType(), false);

			newTerms.add(new DanglingObjectImpl(id));

			historyList.addItem(new CreateObjectHistoryItem(id, io.getType()
					.getID()));
			HistoryGenerator.getTermTextChanges(clone, io, historyList, true,
					null);
			if (io.getNamespace() != null) {
				historyList.addItem(new NamespaceHistoryItem(null, io
						.getNamespace(), id));
			}
			if (io instanceof LinkedObject) {
				Iterator it2 = ((LinkedObject) io).getChildren().iterator();
				while (it2.hasNext()) {
					Link childLink = (Link) it2.next();
					historyList.addItem(new CreateLinkHistoryItem(childLink
							.getChild(), childLink.getType(),
							(LinkedObject) clone));
				}
				it2 = ((LinkedObject) io).getParents().iterator();
				while (it2.hasNext()) {
					Link parentLink = (Link) it2.next();
					historyList.addItem(new CreateLinkHistoryItem(
							(LinkedObject) clone, parentLink.getType(),
							parentLink.getParent()));
				}
			}
			// HistoryGenerator.getParentageChanges(clone, io, historyList);
			HistoryGenerator.getObsoleteChanges(clone, io, historyList, null);

			TermMacroHistoryItem cloneItem = new TermMacroHistoryItem("Clone");
			Iterator it2 = historyList.getHistoryItems();
			while (it2.hasNext()) {
				HistoryItem item = (HistoryItem) it2.next();
				cloneItem.addItem(item);
			}
			itemList.add(cloneItem);
		}
		HistoryItem item;
		if (itemList.size() == 1) {
			item = itemList.get(0);
		} else {
			item = new TermMacroHistoryItem("Cloned multiple terms");
			it = itemList.iterator();
			while (it.hasNext()) {
				HistoryItem subItem = (HistoryItem) it.next();
				((TermMacroHistoryItem) item).addItem(subItem);
			}
		}
		
		GUIUtil.setSelections(item, sources, SelectionManager.createSelection(sources
				.getComponent(), newTerms, null, newTerms.get(0), sources
				.getRootAlgorithm(), sources.getLinkDatabase()));
		return item;
	}
}
