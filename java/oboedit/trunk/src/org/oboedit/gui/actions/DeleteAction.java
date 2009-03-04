package org.oboedit.gui.actions;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PathCapable;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.NameSearchCriterion;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.ObsoleteObjectHistoryItem;
import org.obo.history.RemoveConsiderHistoryItem;
import org.obo.history.RemoveReplacementHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.query.QueryEngine;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.EditAction;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;

import org.apache.log4j.*;

public class DeleteAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DeleteAction.class);

	protected boolean isLegal = false;

	protected KeyStroke keyStroke;

	protected boolean shouldDestroy = false;

	protected boolean deleteTerm = false;

	protected boolean legacyMode = false;

	protected List<PathCapable> deleteThese = new ArrayList<PathCapable>();

	protected int lastInstanceCount = 0;

	protected String instanceString = "";
	protected String wontDelete = "";

	LinkFilterFactory lff = new LinkFilterFactory();
	ObjectFilterFactory off = new ObjectFilterFactory();


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

		logger.debug("DeleteAction: clickInit: selection = " + selection + ", destItem = " + destItem);
		instanceString = "";
		wontDelete = "";
		lastInstanceCount = 0;

		if (selection == null || selection.isEmpty()) {
			isLegal = false;
			return;
		}

		// If user has selected one term but is mousing over a different term, that's not a legal scenario for deletion.
		if (destItem != null && destItem.getTerm() != null &&
				!(selection.getAllSelectedObjects().contains(destItem.getTerm()))) {
			//		    logger.info("Can't delete--" + destItem.getTerm().getName() + " isn't in selection");
			isLegal = false;
			return;
		}
		deleteTerm = false;  // gets set by getDeletionItems
		isLegal = getDeletionItems(selection, deleteThese);
		cullFakeItems(deleteThese);
		if (deleteThese.size() < 1) { // nothing left to delete
			isLegal = false;
		}

		// Can't destroy a relationship.
		if (shouldDestroy && !deleteTerm) {
			isLegal = false;
		}

		//		if (!isLegal)
		//			return;
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

		// What *is* legacyMode really??  It seems to be true whenever the selection
		// was made in the Ontology Tree Editor (rather than in the Graph Editor).
		legacyMode = selection.getSelector() != null
		&& selection.getSelector().hasCombinedTermsAndLinks();

		boolean warnBeforeDelete = Preferences.getPreferences().getWarnBeforeDelete();

		deleteTerm = false;
		for (PathCapable pc : selection.getAllSelectedObjects()) {
			if (pc instanceof LinkedObject) {  // Term (not link)
				LinkedObject lo = (LinkedObject) pc;
				//					logger.info("Checking linkedobject " + lo.getName()); // DEL
				// 3/2008: If node has (non-obsolete) children, don't allow user to delete it.
				if (hasNonObsoleteChildren(lo)) {
					//					    && TermUtil.hasAncestor(lo, lo)) { 
					//					    logger.debug("Can't delete " + lo.getName() + "--it has children: " + lo.getChildren()); // DEL
					wontDelete += lo.getName() + " (" + lo.getID() + ")"
					+ " will not be deleted because it still has children.\n";
				}
				else {  // No children--ok to delete term
					if (!legacyMode || lo.getParents().size() <= 1) { // Sometimes it's 0--??
						//						!(TermUtil.isObsolete(lo.getParents().iterator().next()))) {
						deleteTerm = true;
						//						logger.debug("Can delete " + lo.getName() + ".  children: " + lo.getChildren()); // DEL
						lastInstanceCount++;
						instanceString += lo.getName() + " ("
						+ lo.getID() + ")\n";

						// Delete the parent links for this term
						for (Link parentLink : lo.getParents()) {
							if (!out.contains(parentLink)) {
								out.add(parentLink);
							}
						}
					}
					// It doesn't have children, right?  We already checked.
					//					    for (Link childLink : lo.getChildren()) { // ??
					//						if (!out.contains(childLink))
					//						    out.add(childLink);
					//					    }
					out.add(lo);  // Add the term itself to the delete list
				}
			} else if (pc instanceof Link) {
				Link link = (Link) pc;
				// ! Check whether the link is to null (as for a singleton)?
				if (!TermUtil.isImplied(link) 
						&& link.getType() != null) {  // ?
					// If the child of this link only this one parent, then you're not
					// allowed to delete this link (which would make the child into a root).
					if (link.getChild().getParents().size() > 1) {
						//							logger.info("Adding to delete list: link " + link); // DEL
						out.add(pc);
					}
				}
			}
		}
		return true;
	}

	private boolean hasNonObsoleteChildren(LinkedObject lo) {
		if (lo.getChildren().size() == 0)
			return false;
		for (Link childLink : lo.getChildren()) {
			LinkedObject child = childLink.getChild();
			if (TermUtil.isObsolete(child))
				//		logger.info("Child " + child.getName() + " is obsolete");
				;
			else {
				//		logger.info("Child " + child.getName() + " is NOT obsolete");
				return true;
			}
		}
		return false;
	}

	public HistoryItem execute() {
		if (shouldDestroy) {
			if (JOptionPane
					.showConfirmDialog(
							GUIManager.getManager().getFrame(),
							wontDelete +
							"\nThese terms will be permanently destroyed:\n"
							+ instanceString
							+ "They will be entirely removed from the ontology\n"
							+ "and will not appear as obsolete terms.\nAre "
							+ "you sure you want to proceed?",
							"Destroy warning", JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
				return null;
		} else if (Preferences.getPreferences().getWarnBeforeDelete()) {
			int n = 1; //Place marker to say in this case the term is mentioned in consider or replaced by tags.
			if (lastInstanceCount > 0 //&& !(n==1) 
					) {
				String deleteQuestion = 
					wontDelete +
					((lastInstanceCount > 1) ?
							"\nThese are the last appearances of the following terms:"
							: "\nThis is the last appearance of the following term:")
							+ "\n" + instanceString
							+ "Are you sure you want to make "
							+ ((lastInstanceCount > 1) ?
									"these terms" : "this term")
									+ " permanently obsolete?";
				logger.info(deleteQuestion);
				int answer = JOptionPane.showConfirmDialog(GUIManager.getManager().getFrame(),
						deleteQuestion, "Delete warning",
						JOptionPane.YES_NO_OPTION);
				if (answer != JOptionPane.YES_OPTION) {
					logger.info("User decided not to delete " + instanceString);
					return null;
				}
			}


//			if (lastInstanceCount > 0 && n==1) //This term is the last instance and is 
//				//mentioned in consider or replaced_by tags
//			{
//				logger.debug("DeleteAction: execute: deleteThese = " + deleteThese );
//				String considerTagQuestion = 
//					wontDelete +
//					((lastInstanceCount > 1) ?
//							instanceString + 
//							"\nThese terms are mentioned in replaced_by or consider " +
//							"tags of the following term(s). \nPlease update appropriately."
//							: "\nThis term is mentioned in replaced_by or consider " +
//					"tags of the following term(s). \nPlease update appropriately.\n")
//					+ getObsoletesLinkedToThese().toString()				 
//					+ "\nAre you sure you want to make "
//					+ ((lastInstanceCount > 1) ?
//							"these terms" : "this term")
//							+ " permanently obsolete?";
//				logger.info(considerTagQuestion);
//				int answer = JOptionPane.showConfirmDialog(GUIManager.getManager().getFrame(),
//						considerTagQuestion, "Delete warning",
//						JOptionPane.YES_NO_OPTION);
//				if (answer != JOptionPane.YES_OPTION) {
//					logger.info("User decided not to delete " + instanceString);
//					return null;
//				}
//			}
	}

		Collections.sort(deleteThese, pcComparator);
		logger.info("Candidates for " + (shouldDestroy ? "destruction:" : "deletion: ") + deleteThese);
		TermMacroHistoryItem out = new TermMacroHistoryItem(
		"No items to delete"); // ??
		for (PathCapable pc : deleteThese) {
			if (pc instanceof Link) {
				Link link = (Link) pc;
				//				logger.info(link + " is a Link--adding to " + (shouldDestroy ? "destroy" : "obsolete") + " list"); // DEL
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
			} else if (pc instanceof LinkedObject
					&& (!legacyMode 
							// This test is to allow user to delete singleton terms from OTE
							|| (legacyMode && deleteTerm))) {
				//			    logger.info(pc + " is a LinkedObject--adding to " + (shouldDestroy ? "destroy" : "obsolete") + " list"); // DEL
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
		//		logger.info("Terms to delete: " + out);
		if (out.size() == 0) {
			//		logger.info("DeleteAction.execute: no items to delete"); // DEL
			return null;
		}
		else if (out.size() == 1) {
			//		logger.info("DeleteAction.execute: returning one item to delete: " + out.getItemAt(0)); // DEL
			return out.getItemAt(0);
		}
		else {
			//		logger.info("DeleteAction.execute: returning " + out.size() + " items to delete: " + out); // DEL
			return out;
		}
	}

	/*
	 * Finds out whether any of the terms
	 * to be obsoleted is mentioned in the consider or replaced_by tags of the 
	 * other obsolete terms, and then send the names of the 
	 * obsolete parents back as a string with name and id
	 * together, and a new line for each.
	 * 
	 * The deleteThese collection may contain multiple terms and links, and is like this:
	 * [Child term name A --OBO_REL:is_a--> Parent term name B, Child term name A]
	 */
	public HashMap<String, Collection> getObsoletesLinkedToThese() {

		for (Iterator iterator = deleteThese.iterator(); iterator.hasNext();) {

			String objectType = iterator.next().getClass().getName();
			//String iteratorString = iterator.toString();

			if (!objectType.equals("org.obo.datamodel.impl.OBOClassImpl")){ //If it is a link rather than a term name.

				System.out.println("DeleteAction: getObsoletesLinkedToThese: objectType = " + objectType);
				//System.out.println("DeleteAction: getObsoletesLinkedToThese: iteratorString = " + iteratorString);
				iterator.remove();
			}
		}
		System.out.println("DeleteAction: getObsoletesLinkedToThese: deleteThese = " + deleteThese.toString());

		HashMap<String, Collection> obsoleteTermsLinkedToAllTerms = new HashMap<String, Collection>();

		for (Iterator iterator = deleteThese.iterator(); iterator.hasNext();) {
			//System.out.println("DeleteAction: getObsoletesLinkedToThese: iterator.next().toString() = " + iterator.next().toString());

			String termToBeObsoleted = iterator.next().toString();
			System.out.println("DeleteAction: getObsoletesLinkedToThese: termToBeObsoleted = " + termToBeObsoleted);

			ObjectFilter ofilter = getObjectFilter(termToBeObsoleted);

			System.out.println("DeleteAction: getObsoletesLinkedToThese: ofilter = " + ofilter);

			Collection<Object> obsoleteTermsLinkedToOneTerm = filterLinks(ofilter);

			System.out.println("DeleteAction: getObsoletesLinkedToThese: So far so good.");

			obsoleteTermsLinkedToAllTerms.put(termToBeObsoleted, obsoleteTermsLinkedToOneTerm);

			//	logger.debug(DeleteAction: getObsoletesLinkedToThese: obsoleteTermsLinkedToAllTerms = " + obsoleteTermsLinkedToAllTerms");

		}

		System.out.println("DeleteAction: getObsoletesLinkedToThese: obsoleteTermsLinkedToAllTerms = " + obsoleteTermsLinkedToAllTerms.toString());
		return obsoleteTermsLinkedToAllTerms;
	}



	/*
	 * Creates a custom filter object to find obsolete terms that are parents of the term or terms selected for obsoletion.
	 * The filter may not yet be correct. 
	 * 
	 * Filter produced looks like this: Link child has Any text field equals "[name of term to be obsoleted]"
	 */
	public ObjectFilter getObjectFilter(String id) {


		ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
		System.out.println("FilterTest: getLinkFilter: ofilter = " + ofilter);

		EqualsComparison c = new EqualsComparison();
		System.out.println("FilterTest: getLinkFilter: c = " + c);

		ofilter.setComparison(c);
		System.out.println("FilterTest: getLinkFilter: c = " + c);

		ofilter.setValue(id);
		ofilter.setCriterion(new NameSearchCriterion());

		System.out.println("FilterTest: getLinkFilter: ofilter = " + ofilter);
		LinkFilter lfilter = (LinkFilter)lff.createNewFilter();
		lfilter.setFilter(ofilter);
		return ofilter;

	}


/* 
 * Takes the filter set up in the calling method, which is 'Name equals "[name of term selected for obsoletion"'
 * Calls the OBOSession link database and looks at all the links to see if the child term in any of the links
 * matches that fiter. 
 * 
 * This is 
 */
	public Collection<Object> filterLinks(Filter filter) {

		final Collection<Object> matches = new HashSet<Object>();
		LinkDatabase ldb = SessionManager.getManager().getSession().getLinkDatabase();
		Iterator<Link> it = TermUtil.getAllLinks(ldb);{

			while (it.hasNext()) {

				System.out.println("it is " + it);
				Link link = (Link) it.next();
				System.out.println("link is " + link);

				if (filter.satisfies(link.getChild())){
					matches.add(link.getParent());
				}
			}
		}
		return matches;
	}

	public KeyStroke getKeyStroke() {
		return keyStroke;
	}

	public String getName() {
		if (shouldDestroy) {
			//		    return "Destroy " + ((deleteTerm) ? "term" : "relationship");
			if (deleteTerm)
				return "Destroy term";
			else
				//		    logger.info("DeleteAction.getName--stack trace:"); // DEL
				//		    new Throwable().printStackTrace(); // DEL
				return "Destroy"; // It will be disabled anyway, but we have to put *something* here
		}
		else
			return "Delete " + ((deleteTerm) ? "term (make obsolete)" : "relationship");
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
