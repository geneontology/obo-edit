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

	protected KeyStroke keyStroke;

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
		keyStroke = KeyStroke.getKeyStroke(
				KeyEvent.VK_DELETE, (shouldDestroy ? KeyEvent.SHIFT_DOWN_MASK : 0));
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
		// But we would have returned anyway!!  What's really supposed to happen here?
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
		if (legacyMode) // DEL
		    System.out.println("DeleteAction.getDeletionItems: legacyMode is TRUE!");  // DEL
		boolean warnBeforeDelete = Preferences.getPreferences().getWarnBeforeDelete();

		if (legacyMode) {
			Map temp = new HashMap();

			for (Link tr : selection.getLinks()) {
			    System.out.println("DeleteAction: checking link " + tr); // DEL
			    Collection trackedParents = (Collection) temp.get(tr.getChild());
			    // 3/2008: If node has children, don't allow user to delete it.
			    LinkedObject child = tr.getChild();
			    if (child.getChildren().size() > 0
				&& !TermUtil.hasAncestor(child, child)) { // ?
				System.out.println("Can't delete " + child.getName() + "--it has children: " + child.getChildren()); // DEL
			    }
			    else {
				if (warnBeforeDelete && 
				    !(TermUtil.isObsolete(tr.getParent())) && 
				    child.getParents().size() == 1) {
				    lastInstanceCount++;
				    instanceString += child.getName() + " ("
					+ child.getID() + ")\n";
				}

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
			}

			Iterator it = temp.keySet().iterator();
			while (it.hasNext()) {
				LinkedObject lo = (LinkedObject) it.next();
				Collection parents = (Collection) temp.get(lo);
				if (parents.size() == 0)
					out.add(lo);
			}

			return true;
		} else { // Not legacyMode
			for (PathCapable pc : selection.getAllSelectedObjects()) {
				if (pc instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) pc;
					// 3/2008: If node has children, don't allow user to delete it.
					if ((lo.getChildren().size() > 0)) {
//					    && TermUtil.hasAncestor(lo, lo)) {
					    // What if the children are obsolete??
					    System.out.println("Can't delete " + lo.getName() + "--it has children: " + lo.getChildren()); // DEL
					}
					else {  // No children
					    System.out.println(lo.getName() + " has no (real) children: " + lo.getChildren() + "--ok to delete. Parent count = " + lo.getParents().size() + "; parents = " + lo.getParents()); // DEL
					    if (warnBeforeDelete &&
						lo.getParents().size() <= 1) { // Sometimes it's 0--??
//						!(TermUtil.isObsolete(lo.getParents().iterator().next()))) {
						lastInstanceCount++;
						instanceString += lo.getName() + " ("
						    + lo.getID() + ")\n";
//						System.out.println("now instanceString = " + instanceString); // DEL
					    }

					    for (Link parentLink : lo.getParents()) {
						if (!out.contains(parentLink))
						    out.add(parentLink);
					    }
					    for (Link childLink : lo.getChildren()) { // ??
						if (!out.contains(childLink))
						    out.add(childLink);
					    }
					out.add(lo);
					}
				} else if (pc instanceof Link) {
					Link link = (Link) pc;
				    System.out.println("Adding to delete list: link " + link); // DEL
					if (!TermUtil.isImplied(link))
						out.add(pc);
				}
			}
			return true;
		}
	}

	public HistoryItem execute() {
	    // Show "Are you sure you want to destroy?" warning whether it's legacyMode or not.
//		if (legacyMode) {
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
			    // Is this the right test?  Is it failing to warn in some cases where it should?
				if (lastInstanceCount > 0) {
				    int answer = JOptionPane.showConfirmDialog(GUIManager.getManager().getFrame(),
									       ((lastInstanceCount > 1) ?
										"These are the last appearances of the following terms:"
										:
										"This is the last appearance of the following term:")
									       + "\n" + instanceString
									       + "Are you sure you want to "
									       + "make these terms permanently obsolete?", "Delete warning",
									       JOptionPane.YES_NO_OPTION);
				    if (answer != JOptionPane.YES_OPTION) {
					System.out.println("User decided not to delete " + instanceString);
					return null;
				    }
				}
			}
//		}
		Collections.sort(deleteThese, pcComparator);
		System.out.println("Candidates for " + (shouldDestroy ? "destruction:" : "deletion: ") + deleteThese);
		TermMacroHistoryItem out = new TermMacroHistoryItem(
//		    "No items to delete"); // ??
		    );
		for (PathCapable pc : deleteThese) {
			if (pc instanceof Link) {
				Link link = (Link) pc;
				System.out.println(link + " is a Link--adding to " + (shouldDestroy ? "destroy" : "obsolete") + "list"); // DEL
				if (link.getType() == null)
					out.addItem(new DeleteLinkHistoryItem((Link) pc));
				else if (link.getType().equals(OBOProperty.CONSIDER)) {
					out.addItem(new RemoveConsiderHistoryItem(link.getParent()
							.getID(), link.getChild().getID()));
				} else if (link.getType().equals(OBOProperty.REPLACES)) {
					out.addItem(new RemoveReplacementHistoryItem(link.getParent()
							.getID(), link.getChild().getID()));
				} else
					out.addItem(new DeleteLinkHistoryItem((Link) pc));
			} else if (pc instanceof LinkedObject) {
			    System.out.println(pc + " is a LinkedObject--adding to " + (shouldDestroy ? "destroy" : "obsolete") + "list"); // DEL
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
//		System.out.println("Terms to delete: " + out);
		if (out.size() == 0) {
		    System.out.println("DeleteAction.execute: no items to delete"); // DEL
			return null;
		}
		else if (out.size() == 1) {
		    System.out.println("DeleteAction.execute: returning one item to delete: " + out.getItemAt(0)); // DEL
			return out.getItemAt(0);
		}
		else {
		    System.out.println("DeleteAction.execute: returning " + out.size() + " items to delete: " + out); // DEL
			return out;
		}
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
