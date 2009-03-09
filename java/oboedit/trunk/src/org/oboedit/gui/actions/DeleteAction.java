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
import javax.swing.JTable;
import javax.swing.KeyStroke;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.OBOClassImpl;
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
import org.obo.history.SessionHistoryList;
import org.obo.history.TermMacroHistoryItem;
import org.obo.query.QueryEngine;
import org.obo.query.impl.SearchHit;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractSearchResultsTableModel;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.EditAction;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.SearchResultsTable;
import org.oboedit.gui.SearchResultsTableModel;
import org.oboedit.gui.Selection;

import org.apache.log4j.*;

/**
 * <p>
 * @author Jennifer I Deegan and John Day-Richter
 * </p><p>
 * Enables the user to destroy or obsolete terms. Methods are included to ensure that if terms to be obsoleted are mentioned in 
 * consider or replaced_by tags then this information will be displayed in the dialogue box, along a warning that the 
 * the user is deleting the last relationship between the term and any parent and that obsoletion will occur.</p>
 *</p>
 */
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
	boolean usedInReplacementTag = false;

	public boolean getUsedInReplacementTag() {
		return usedInReplacementTag;
	}

	public void setUsedInReplacementTag(boolean usedInReplacementTag) {
		this.usedInReplacementTag = usedInReplacementTag;
	}

	String termToBeObsoleted;



	HashMap<String, Collection> obsoleteTermsLinkedToAllTerms;



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

		if (shouldDestroy) { //if the term is to be destroyed rather than obsoleted.
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

			//Find out if the terms being obsoleted are mentioned in any replaced_by or consider terms.
			//Reformat the list for display in the dialog box.
			//This also sets a boolean that will be used to determine whether to display the dialog box that includes information 
			//about replced_by tags.
			setUsedInReplacementTag(false);  //Reset boolean in case it was previously set to true by another obsoletion.
			String replacedMessage = getObsoletesLinkedToThese().toString();
			replacedMessage = replacedMessage.replace("{", "\n\n");
			replacedMessage = replacedMessage.replace("=[", ": ");
			replacedMessage = replacedMessage.replace("]}", "\n");
			replacedMessage = replacedMessage.replace(",", "\n");
			replacedMessage = replacedMessage.replace("]", "");

			//If this is the last instance of the term(s) to be obsoleted, and it is (they are) not 
			//referenced in any replaced_by or consider tags. 
			if (lastInstanceCount > 0 && !getUsedInReplacementTag()) {
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
					setUsedInReplacementTag(false);
					return null;
				}
			}

			//If this is the last instance of the term(s) to be obsoleted, and it is (they are) 
			//referenced in any replaced_by or consider tags. 
			if (lastInstanceCount > 0  && getUsedInReplacementTag()) 
			{
				logger.debug("DeleteAction: execute: deleteThese = " + deleteThese );
				String considerTagQuestion = 
					//wontDelete +
					((lastInstanceCount > 1) ?
							//instanceString + 
							"These are the last appearances of the selected terms. " +
							"Please update the replaced_by or consider tags that reference these terms:"
							: "This is the last appearance of the selected term. " +
					"Please update the replaced_by or consider tags that reference this term:")
					+ replacedMessage 
					+ "\nAre you sure you want to make "
					+ ((lastInstanceCount > 1) ?
							"these terms" : "this term")
							+ " permanently obsolete?";
				logger.info(considerTagQuestion);

				int answer = JOptionPane.showConfirmDialog(GUIManager.getManager().getFrame(),
						considerTagQuestion, "Delete warning",
						JOptionPane.YES_NO_OPTION);
				if (answer != JOptionPane.YES_OPTION) {
					logger.info("User decided not to delete " + instanceString);
					setUsedInReplacementTag(false);
					return null;
				}
			}
		}



		//This part does the obsoletion. 
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

	/**
	 * <p>Finds a list of previously obsoleted terms referencing those terms being obsoleted by the user. 
	 * </p><p>
	 * This method finds the names of each of the terms chosen by the user for obsoletion. 
	 *  </p><p>
	 * It loops through these terms calling the getObjectFilter and filterLinks methods for each. 
	 * </p><p>
	 * The getObjectFilter method makes a filter that will check any term name to see if it corresponds to 
	 *  selected term (in this case a term name that the user selected for obsoletion). 
	 * 
	 * </p><p>
	 * The filterLinks method is then called.
	 * 
	 *  </p><p>
	 * In it, the SessionManager is used to get the linkDatabase, and then TermUtil is used
	 * to get the complete set of obsolete terms. 
	 * </p><p>
	 * The method 
	 * iterates through each of the total set of previously obsoleted terms. For each previously 
	 * obsoleted term it gets the set of consider and replaced_by tags and checks to see if the child attached to 
	 * this tag matches the filter 
	 * made in getObjectFilter. That is, it checks to see if any of the term names referenced in a pre-existing 
	 * consider or replaced_by tag matches the name of the term that the user is trying to obsolete.
	 * </p><p>
	 * If the term slated for obsoletion is attached to a consider or replaced_by tag,
	 *  then the name of the previously obsoleted term or terms that carries the
	 * consider or replaced_by tag will be returned to this current method 
	 * in a Hashset. Back in the calling method, each HashSet is then entered as a value in a 
	 * HashMap with the key being the name of the term that the user is obsoleting. 
	 * </p><p>
	 * HashMap:<br>
	 * key = name of term the user is trying to obsolete,
	 * value = list of previously obsoleted terms that reference this term via
	 * a consider or replaced_by tag
	 * </p><p>
	 * The HashMap is returned so that the toString return value can be formatted for display in a dialog box.
	 * </p><p>
	 * For reference, the deleteThese collection may contain multiple terms and links, and is like this:</p><p>
	 * [Child term name A --OBO_REL:is_a--> Parent term name B, Child term name A]
	 * </p><p>
	 * This collection is copied and the copy is modified to get the list of terms the user wishes to obsolete. </p>
	 */
	public HashMap<String, Collection> getObsoletesLinkedToThese() {

		List<PathCapable> modifiedDeleteThese = new ArrayList<PathCapable>();

		//Take a copy of the collection with the list of terms to be obsoleted. The collection contains links and term names
		//and we only want the term names. 
		modifiedDeleteThese.addAll(deleteThese);

		for (Iterator iterator = modifiedDeleteThese.iterator(); iterator.hasNext();) {

			String objectType = iterator.next().getClass().getName();

			//Find the links and delete them. 
			if (!objectType.equals("org.obo.datamodel.impl.OBOClassImpl")){ //If it is a link rather than a term name.
				//logger.debug("DeleteAction: getObsoletesLinkedToThese: objectType = " + objectType);
				////logger.debug("DeleteAction: getObsoletesLinkedToThese: iteratorString = " + iteratorString);
				iterator.remove();
			}
		}
		//logger.debug("DeleteAction: getObsoletesLinkedToThese: modifiedDeleteThese = " + modifiedDeleteThese.toString());

		obsoleteTermsLinkedToAllTerms = new HashMap<String, Collection>();

		for (Iterator iterator = modifiedDeleteThese.iterator(); iterator.hasNext();) {
			//logger.debug("DeleteAction: getObsoletesLinkedToThese: iterator.next().toString() = " + iterator.next().toString());

			//Get the name of the term.
			termToBeObsoleted = iterator.next().toString();
			//logger.debug("DeleteAction: getObsoletesLinkedToThese: termToBeObsoleted = " + termToBeObsoleted);

			//Get the filter that will search for 'Name equals "[name of term selected]"'
			ObjectFilter ofilter = getObjectFilter(termToBeObsoleted);
			//logger.debug("DeleteAction: getObsoletesLinkedToThese: ofilter = " + ofilter);

			//Run the filter, for this one term that we want to obsolete, on the entire set of obsolete terms. 
			Collection<Object> obsoleteTermsLinkedToOneTerm = filterLinks(ofilter);
			//logger.debug("DeleteAction: getObsoletesLinkedToThese: So far so good.");

			//if condition included so that if only one of two or more terms to be obsoleted is not included in any 
			//consider of replaced_by tags, then it will not be shown in the dialog box. 
			if(!obsoleteTermsLinkedToOneTerm.isEmpty()){
				obsoleteTermsLinkedToAllTerms.put(termToBeObsoleted, obsoleteTermsLinkedToOneTerm);
				//	logger.debug(DeleteAction: getObsoletesLinkedToThese: obsoleteTermsLinkedToAllTerms = " + obsoleteTermsLinkedToAllTerms");
			}
		}

		//logger.debug("DeleteAction: getObsoletesLinkedToThese: obsoleteTermsLinkedToAllTerms = " + obsoleteTermsLinkedToAllTerms.toString());


		return obsoleteTermsLinkedToAllTerms;


	}



	/**<p>
	 * Creates a custom filter object to find terms whose name matches the name of the term that the user has selected.
	 * </p><p>
	 * Filter produced looks like this: Name equals "[name of selected term ]"</p>
	 */
	public ObjectFilter getObjectFilter(String id) {


		ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
		//logger.debug("FilterTest: getLinkFilter: ofilter = " + ofilter);

		EqualsComparison c = new EqualsComparison();
		//logger.debug("FilterTest: getLinkFilter: c = " + c);

		ofilter.setComparison(c);
		//logger.debug("FilterTest: getLinkFilter: c = " + c);

		ofilter.setValue(id);
		ofilter.setCriterion(new NameSearchCriterion());

		logger.debug("FilterTest: getLinkFilter: ofilter = " + ofilter);
		LinkFilter lfilter = (LinkFilter)lff.createNewFilter();
		lfilter.setFilter(ofilter);
		return ofilter;

	}


	/**
	 * See docs for getObsoletesLinkedToThese() from which this method is called. 
	 * @param filter
	 * @return matches
	 */
	
	public Collection<Object> filterLinks(Filter filter) {

		Collection<Object> matches = new HashSet<Object>();
		
		//This gets the list of obsolete terms and makes an iterator so we can look through and check if any of the 
		//terms we are obsoleting are mentioned in the consider or replaced_by tags. 
		//TODO: It would be better if this was not done every time in the iteration, but was done once and reused, but I have not fixed this yet.
		LinkDatabase ldb = SessionManager.getManager().getSession().getLinkDatabase();
		Collection<ObsoletableObject> allObsoletes = TermUtil.getObsoletes(ldb);
		Iterator obsoletesIterator = allObsoletes.iterator();


		while (obsoletesIterator.hasNext()) {

			//logger.debug("DeleteAction: filterLinks: it = " + obsoletesIterator);
			OBOClassImpl obsoleteObject = (OBOClassImpl) obsoletesIterator.next();
			//logger.debug("DeleteAction: filterLinks: ObsoleteObject is " + obsoleteObject);

			//logger.debug("DeleteAction: filterLinks: obsoleteObject.getReplacedBy() " + obsoleteObject.getReplacedBy());

			//For each obsolete term get the set of terms linked by consider or replaced_by tags. 
			Set replacedBySet = obsoleteObject.getReplacedBy();
			Set considerSet = obsoleteObject.getConsiderReplacements();


			for (Iterator replacementsIterator = replacedBySet.iterator(); replacementsIterator.hasNext();) {
				Object replacementTerm = (Object) replacementsIterator.next();

				if (filter.satisfies(replacementTerm)){
					setUsedInReplacementTag(true);
					System.out.println(usedInReplacementTag);
					//logger.debug("DeleteAction: filterLinks: replacementTerm " + replacementTerm);
					matches.add(obsoleteObject);
				}
			}

			for (Iterator considerIterator = considerSet.iterator(); considerIterator.hasNext();) {
				Object considerTerm = (Object) considerIterator.next();

				if (filter.satisfies(considerTerm)){
					setUsedInReplacementTag(true);
					//logger.debug("DeleteAction: filterLinks: considerTerm " + considerTerm);
					matches.add(obsoleteObject);
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
