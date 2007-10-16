package org.oboedit.gui.actions;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.ObsoleteObjectHistoryItem;
import org.obo.history.RemoveConsiderHistoryItem;
import org.obo.history.RemoveReplacementHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.TermUtil;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.EditAction;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;

public class DeleteAction implements ClickMenuAction {

	protected boolean isLegal = false;

	protected static KeyStroke keyStroke = KeyStroke.getKeyStroke(
			KeyEvent.VK_DELETE, 0);

	protected boolean shouldDestroy = false;

	protected boolean legacyMode = false;

	protected List<PathCapable> deleteThese = new ArrayList<PathCapable>();

	protected int lastInstanceCount = 0;

	protected String instanceString = "";

	protected Comparator<PathCapable> pcComparator = new Comparator<PathCapable>() {
		public int compare(PathCapable o1, PathCapable o2) {
			if (o1 instanceof Link)
				return -1;
			else if (o2 instanceof Link)
				return 1;
			else
				return 0;
		}
	};

	public DeleteAction(boolean shouldDestroy) {
		setShouldDestroy(shouldDestroy);
	}

	public void setShouldDestroy(boolean shouldDestroy) {
		this.shouldDestroy = shouldDestroy;
	}

	public void clickInit(Selection selection, GestureTarget destItem) {
		instanceString = "";
		lastInstanceCount = 0;

		if (selection == null || selection.isEmpty()) {
			isLegal = false;
			return;
		}
		isLegal = getDeletionItems(selection, deleteThese);
		cullFakeItems(deleteThese);
		if (deleteThese.size() < 1) {
			isLegal = false;
		}
		if (!isLegal)
			return;
	}

	protected void cullFakeItems(List<PathCapable> deleteThese) {
		Iterator it = deleteThese.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof Link) {
				Link l = (Link) o;
				if (TermUtil.isImplied(l)) {
					it.remove();
				}
			}
		}
	}

	protected boolean getDeletionItems(Selection selection,
			Collection<PathCapable> out) {
		deleteThese.clear();
		legacyMode = selection.getSelector() != null
				&& selection.getSelector().hasCombinedTermsAndLinks();
		if (legacyMode) {
			if (Preferences.getPreferences().getWarnBeforeDelete()) {
				for (Link tr : selection.getLinks()) {
					LinkedObject parent = tr.getParent();
					LinkedObject child = tr.getChild();
					if (TermUtil.isObsolete(parent))
						continue;
					if (child.getParents().size() == 1) {
						lastInstanceCount++;
						instanceString += child.getName() + " ("
								+ child.getID() + ")\n";
					}
				}
			}
			Map temp = new HashMap();

			for (Link tr : selection.getLinks()) {
				Collection trackedParents = (Collection) temp
						.get(tr.getChild());
				if (trackedParents == null) {
					trackedParents = new LinkedList<Link>(tr.getChild()
							.getParents());
					if (trackedParents.size() == 0) {
						out.add(tr.getChild());
						continue;
					} else
						temp.put(tr.getChild(), trackedParents);
				}
				out.add(tr);
				trackedParents.remove(tr);
			}

			Iterator it = temp.keySet().iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				Collection parents = (Collection) temp.get(lo);
				if (parents.size() == 0)
					out.add(lo);
			}

			return true;
		} else {
			for (PathCapable pc : selection.getAllSelectedObjects()) {
				if (pc instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) pc;
					for (Link parentLink : lo.getParents()) {
						if (!out.contains(parentLink))
							out.add(parentLink);
					}
					for (Link childLink : lo.getChildren()) {
						if (!out.contains(childLink))
							out.add(childLink);
					}
					out.add(lo);
				} else if (pc instanceof Link) {
					Link link = (Link) pc;
					if (!TermUtil.isImplied(link))
						out.add(pc);
				}
			}
			return true;
		}
	}

	public HistoryItem execute() {
		if (legacyMode) {
			if (shouldDestroy) {
				if (JOptionPane
						.showConfirmDialog(
								GUIManager.getManager().getFrame(),
								"The terms will be permanently destroyed.\n"
										+ "They "
										+ "will be entirely removed from the ontology\n"
										+ "and will not appear as obsolete terms.\nAre "
										+ "you sure you want to proceed?",
								"Destroy warning", JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
					return null;
			} else {
				if (lastInstanceCount > 0) {
					if (JOptionPane.showConfirmDialog(GUIManager.getManager()
							.getFrame(), "These are the last appearances "
							+ "the following terms\n" + instanceString
							+ "Are you sure you want to "
							+ "permanently remove these terms from "
							+ "the ontology?", "Delete warning",
							JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
						return null;
				}
			}
		}
		Collections.sort(deleteThese, pcComparator);
		TermMacroHistoryItem out = new TermMacroHistoryItem(
				"Deleted multiple items");
		for (PathCapable pc : deleteThese) {
			if (pc instanceof Link) {
				Link link = (Link) pc;
				if (link.getType().equals(OBOProperty.CONSIDER)) {
					out.addItem(new RemoveConsiderHistoryItem(link.getParent()
							.getID(), link.getChild().getID()));
				} else if (link.getType().equals(OBOProperty.REPLACES)) {
					out.addItem(new RemoveReplacementHistoryItem(link.getParent()
							.getID(), link.getChild().getID()));
				} else
					out.addItem(new DeleteLinkHistoryItem((Link) pc));
			} else if (pc instanceof LinkedObject) {
				if (shouldDestroy)
					out
							.addItem(new DestroyObjectHistoryItem(
									(LinkedObject) pc));
				else
					out
							.addItem(new ObsoleteObjectHistoryItem(
									(LinkedObject) pc));
			}
		}
		if (out.size() == 0)
			return null;
		else if (out.size() == 1)
			return out.getItemAt(0);
		else
			return out;
	}

	public KeyStroke getKeyStroke() {
		return keyStroke;
	}

	public String getName() {
		if (shouldDestroy)
			return "Destroy";
		else
			return "Delete";
	}

	public String getDesc() {
		if (shouldDestroy)
			return "Destroying";
		else
			return "Deleting";
	}

	public List<EditAction> getSubActions() {
		return null;
	}

	public boolean isLegal() {
		return isLegal;
	}

}
