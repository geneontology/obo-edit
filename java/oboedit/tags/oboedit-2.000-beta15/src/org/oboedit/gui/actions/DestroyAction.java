package org.oboedit.gui.actions;

import org.bbop.framework.GUIManager;

import java.awt.event.*;
import java.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.tree.TreePath;
import javax.swing.*;

public class DestroyAction implements ClickMenuAction {

	protected Selection sources;

	protected List<LinkedObject> deleteThese = new Vector<LinkedObject>();

	protected GestureTarget destItem;

	protected boolean isLegal = false;

	protected static KeyStroke keyStroke = KeyStroke.getKeyStroke(
			KeyEvent.VK_DELETE, InputEvent.CTRL_MASK);

	public KeyStroke getKeyStroke() {
		return keyStroke;
	}

	public String getName() {
		return "Destroy";
	}

	public String getDesc() {
		return "Destroy";
	}

	public List<EditAction> getSubActions() {
		return null;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = sources.getTerms().size() > 0;
		this.sources = sources;
		deleteThese.clear();
		for (LinkedObject child : sources.getTerms()) {
			deleteThese.add(child);

			if (child.getParents().size() > 1) {
				isLegal = false;
				return;
			}

			if (child.getChildren().size() > 0
					&& !TermUtil.hasAncestor(child, child)) {
				isLegal = false;
				return;
			}

			if (child.isBuiltIn()) {
				isLegal = false;
				return;
			}

			if (TermUtil.isProperty(child)) {
				Iterator it = TermUtil.getTerms(
						SessionManager.getManager().getSession()).iterator();
				while (it.hasNext()) {
					LinkedObject term = (LinkedObject) it.next();
					if (TermUtil.usesType(term, (OBOProperty) child)) {
						isLegal = false;
						return;
					}
				}
				it = TermUtil.getRelationshipTypes(
						SessionManager.getManager().getSession()).iterator();
				while (it.hasNext()) {
					LinkedObject term = (LinkedObject) it.next();
					if (TermUtil.usesType(term, (OBOProperty) child)) {
						isLegal = false;
						return;
					}
				}
			}
		}
		this.sources = sources;
		this.destItem = null;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		if (JOptionPane.showConfirmDialog(GUIManager.getManager().getFrame(),
				"The terms will be permanently destroyed.\n" + "They "
						+ "will be entirely removed from the ontology\n"
						+ "and will not appear as obsolete terms.\nAre "
						+ "you sure you want to proceed?", "Destroy warning",
				JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
			return null;

		List<HistoryItem> itemList = new Vector<HistoryItem>();
		Iterator it = deleteThese.iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			Iterator it2 = tr.getChild().getParents().iterator();
			while (it2.hasNext()) {
				Link parent = (Link) it2.next();
				itemList.add(new DeleteLinkHistoryItem(parent));
			}
			HistoryItem item = new DestroyObjectHistoryItem(tr.getChild());
			itemList.add(item);
		}
		HistoryItem item;
		if (itemList.size() == 1) {
			item = (HistoryItem) itemList.get(0);
		} else {
			item = new TermMacroHistoryItem("Destroyed multiple terms");
			it = itemList.iterator();
			while (it.hasNext()) {
				HistoryItem subItem = (HistoryItem) it.next();
				((TermMacroHistoryItem) item).addItem(subItem);
			}
		}
		GUIUtil.setSelections(item, sources, SelectionManager
				.createEmptySelection(null));
		return item;
	}
}
