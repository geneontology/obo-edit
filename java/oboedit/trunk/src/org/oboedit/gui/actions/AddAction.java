package org.oboedit.gui.actions;

import org.bbop.framework.GUIManager;

import java.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.DanglingLinkImpl;
import org.obo.history.*;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.tree.TreePath;
import javax.swing.*;

public class AddAction implements ClickMenuAction {

	protected LinkedObject target;

	protected TreePath dest;

	protected boolean isLegal = false;

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Create new child";
	}

	public String getDesc() {
		return "Create new child";
	}

	public List getSubActions() {
		return null;
	}

	public void clickInit(Selection paths, GestureTarget destItem) {
		isLegal = false;
		if (paths.getTerms().size() == 1) {
			target = paths.getTerms().iterator().next();
			if (!TermUtil.isObsolete(target)) {
				isLegal = true;
			}
			dest = paths.getPath();
		}
	}

	protected String getTypeID() {
		String id = JOptionPane.showInputDialog("Please input an id");
		if (id == null || id.length() == 0) {
			JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
					"Cannot create a new type " + "without an id");
			return null;
		} else {
			if (SessionManager.getManager().getSession().getObject(id) != null) {
				JOptionPane.showMessageDialog(GUIManager.getManager()
						.getFrame(), "Id " + id + " already in use!");
				return null;
			} else if (!IDUtil.isLegalID(id)) {
				JOptionPane.showMessageDialog(GUIManager.getManager()
						.getFrame(), "Id " + id
						+ " contains illegal characters");
				return null;
			}
		}
		return id;
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		String id = null;
		if (TermUtil.isProperty(target))
			id = getTypeID();
		else
			id = GUIUtil.fetchID(target);
		if (id == null || id.trim().length() == 0) {
			JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
					"Could not generate ID! " + "Action cancelled.");
			return null;
		}

		TreePath[] preSelection = { dest };
		TreePath[] postSelection = { dest
				.pathByAddingChild(new DanglingLinkImpl(id, OBOProperty.IS_A
						.getID(), target.getID())) };

		TermMacroHistoryItem item = new TermMacroHistoryItem(
				"Add new child to " + target);
		item.addItem(new CreateObjectHistoryItem(id, target.getType().getID()));
		item.addItem(new CreateLinkHistoryItem(id, OBOProperty.IS_A.getID(),
				target.getID()));
		item.addItem(new NameChangeHistoryItem("<new term>", id, id));

		Namespace ns = target.getNamespace();
		if (ns == null)
			ns = SessionManager.getManager().getSession().getDefaultNamespace();
		if (ns != null) {
			item.addItem(new NamespaceHistoryItem(null, ns, id));
		}
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
		item.setTarget(target.getID());
		item.setResult(id);

		GUIUtil.setSelections(item, SelectionManager.createSelectionFromPaths(
				null, preSelection, null, SessionManager.getManager()
						.getCurrentLinkDatabase(), RootAlgorithm.GREEDY, true),
				SelectionManager.createSelectionFromPaths(null, postSelection,
						null, SessionManager.getManager()
								.getCurrentLinkDatabase(),
						RootAlgorithm.GREEDY, true));

		return item;
	}
}
