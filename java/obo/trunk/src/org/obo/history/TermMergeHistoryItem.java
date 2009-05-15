package org.obo.history;

import org.bbop.util.*;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class TermMergeHistoryItem extends SubclassedMacroHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermMergeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 153351404511268849L;

	protected String slave;

	protected List graphEdits = new Vector();

	public TermMergeHistoryItem(OBOClass masterNode, OBOClass slaveNode) {
		this(masterNode.getID(), slaveNode.getID());
	}

	public TermMergeHistoryItem() {
		this((String) null, null);
	}

	public TermMergeHistoryItem(String masterNode, String slaveNode) {
		super("merge");
		setTarget(masterNode);
		this.slave = slaveNode;
	}

	@Override
	public int hashCode() {
		// ^ is bitwise exclusive OR
		return getHash(target) ^ getHash(slave);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermMergeHistoryItem))
			return false;
		TermMergeHistoryItem item = (TermMergeHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
		&& ObjectUtil.equals(slave, item.getSlave());
	}

	@Override
	protected OperationWarning getItems(OBOSession history, List out) {
		IdentifiedObject m = history.getObject(target);
		IdentifiedObject sio = history.getObject(slave);

		if (m == null) {
			return new OperationWarning("Could not merge unrecognized node "
					+ target);
		} else if (sio == null) {
			return new OperationWarning("Could not merge unrecognized node "
					+ slave);
		}
		if (!(m instanceof OBOClass))
			return new OperationWarning("Could not merge non-class node "
					+ target);
		if (!(sio instanceof OBOClass))
			return new OperationWarning("Could not merge non-class node "
					+ slave);

		OBOClass masterNode = (OBOClass) m;
		OBOClass slaveNode = (OBOClass) sio;

		//Check if the term to be subsumed is mentioned in the substitution tags of any obsolete terms, and 
		//update appropriately. 
		checkSubstitutionTags(out, slaveNode, masterNode, history);


		Collection targetDescendants = TermUtil
		.getDescendants(masterNode, true);
		Collection targetAncestors = TermUtil.getAncestors(masterNode, true);

		// remove the slave node as parents of slave node's children,
		// and add them to the master node
		Iterator it = slaveNode.getChildren().iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();

			LinkedObject slaveChild = tr.getChild();

			Link newtr = new OBORestrictionImpl(tr.getParent(), tr.getType(),
					slaveChild);

			out.add(new DeleteLinkHistoryItem(tr));

			if (targetAncestors.contains(slaveChild)) {
				continue;
			}
			if (!HistoryUtil.hasChild(masterNode, tr))
				out.add(new CreateLinkHistoryItem(slaveChild, tr
						.getType(), masterNode));
		}

		// remove the slave node from all slave parents, and replace it
		// with the master node
		Vector slaveParents = new Vector();
		slaveParents.addAll(slaveNode.getParents());
		for (int i = 0; i < slaveParents.size(); i++) {
			Link tr = (Link) slaveParents.get(i);

			LinkedObject slaveParent = tr.getParent();

			Link newtr = new OBORestrictionImpl(masterNode, tr.getType(), tr
					.getParent());

			out.add(new DeleteLinkHistoryItem(tr));

			if (targetDescendants.contains(slaveParent)) {
				continue;
			}

			if (!HistoryUtil.hasChild(tr.getParent(), newtr)) {
				out.add(new CreateLinkHistoryItem(masterNode, tr
						.getType(), tr.getParent()));
			}
		}

		out.add(new DestroyObjectHistoryItem(slaveNode));

		// add a secondary id
		out.add(new SecondaryIDHistoryItem(masterNode, slaveNode.getID(),
				false));
		it = slaveNode.getSecondaryIDs().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			out.add(new SecondaryIDHistoryItem(masterNode, id, false));
		}

		it = slaveNode.getSynonyms().iterator();
		while (it.hasNext()) {
			Synonym s = (Synonym) it.next();
			Synonym masters = HistoryUtil.findSynonym(masterNode, s.getText());
			if (masters == null) {
				out.add(new AddSynonymHistoryItem(masterNode.getID(), s
						.getText()));
				out.add(new ChangeSynScopeHistoryItem(masterNode.getID(), s
						.getText(), Synonym.RELATED_SYNONYM, s.getScope()));
				masters = DefaultObjectFactory.getFactory().createSynonym(
						s.getText(), Synonym.RELATED_SYNONYM);
			}
			Iterator it2 = s.getXrefs().iterator();
			while (it2.hasNext()) {
				Dbxref ref = (Dbxref) it2.next();
				if (!masters.getXrefs().contains(ref)) {
					out.add(new AddDbxrefHistoryItem(masterNode.getID(), ref,
							false, masters.getText()));
				}
			}
		}

		it = slaveNode.getDbxrefs().iterator();
		while (it.hasNext()) {
			Dbxref ref = (Dbxref) it.next();
			if (!masterNode.getDbxrefs().contains(ref)) {
				out.add(new AddDbxrefHistoryItem(masterNode.getID(), ref,
						false, null));
			}
		}

		Synonym s = HistoryUtil.findSynonym(masterNode, slaveNode.getName());
		if (s == null) {
			out.add(new AddSynonymHistoryItem(masterNode.getID(), slaveNode
					.getName()));
			out
			.add(new ChangeSynScopeHistoryItem(masterNode.getID(),
					slaveNode.getName(), Synonym.RELATED_SYNONYM,
					Synonym.EXACT_SYNONYM));
		}
		String newDef;
		String newComment;

		if (masterNode.getDefinition().length() == 0) {
			newDef = slaveNode.getDefinition();
		} else if (slaveNode.getDefinition().length() == 0) {
			newDef = masterNode.getDefinition();
		} else {
			newDef = "MERGED DEFINITION:\n" + "TARGET DEFINITION: "
			+ masterNode.getDefinition() + "\n"
			+ "--------------------\n" + "SOURCE DEFINITION: "
			+ slaveNode.getDefinition();
		}

		if (masterNode.getComment().length() == 0) {
			newComment = slaveNode.getComment();
		} else if (slaveNode.getComment().length() == 0) {
			newComment = masterNode.getComment();
		} else {
			newComment = "MERGED COMMENT:\n" + "TARGET COMMENT: "
			+ masterNode.getComment() + "\n" + "--------------------\n"
			+ "SOURCE COMMENT: " + slaveNode.getComment();
		}
		out.add(new DefinitionChangeHistoryItem(masterNode, newDef));
		out.add(new CommentChangeHistoryItem(masterNode, newComment));


		return null;
	}




	/**
	 * Checks the obsolete terms to see if the term being subsumed in a merge is being used in any substitution tags.
	 * If it is, then methods are called with the effect that the substitution tag is removed and replaced with a substitution tag pointing 
	 * to the subsuming term in the merge. 
	 * 
	 * @param out List of HistoryItems.
	 * @param slaveNode The term being subsumed.
	 * @param masterNode The term subsuming. 
	 * @param history The OBOSession. 
	 */
	private void checkSubstitutionTags(List out, OBOClass slaveNode,
			OBOClass masterNode, OBOSession history) {

		//Get the obsoletes in the whole ontology.
		LinkDatabase ldb = history.getLinkDatabase();
		Collection<ObsoletableObject> obsoletes = TermUtil.getObsoletes(ldb);
		//Iterate through them. 
		for (Iterator obsoletesIterator = obsoletes.iterator(); obsoletesIterator.hasNext();) {
			ObsoletableObject obsoleteTerm = (ObsoletableObject) obsoletesIterator.next();

			//For each obsolete, get the collection of its consider terms. 
			Collection considerTerms = obsoleteTerm.getConsiderReplacements();
			//Iterate through the collection of consider terms for each obsolete in the file
			for (Iterator obsoleteTermConsiderTagsIterator = considerTerms.iterator(); obsoleteTermConsiderTagsIterator.hasNext();) {
				ObsoletableObject considerTerm = (ObsoletableObject) obsoleteTermConsiderTagsIterator.next();

				System.out.println("TermMergeHistoryItem: obsoletes section: " +
						"considerTerm = " + considerTerm +
						" slaveNode = " + slaveNode);		

				//If the term that is being subsumed in the merge is mentioned in the collection
				// of consider terms for the obsolete term currently being examined, then call this method. 
				if (slaveNode == considerTerm){
					updateConsiderReplacementOnMerge(out, obsoleteTerm, slaveNode, masterNode);
				}
			}

			Collection replacementTerms =  obsoleteTerm.getReplacedBy();
			//Iterate through the collection of replaced_by terms for each obsolete in the file
			for (Iterator obsoleteTermReplacedByIterator = replacementTerms.iterator(); obsoleteTermReplacedByIterator.hasNext();) {
				ObsoletableObject replacementTerm = (ObsoletableObject) obsoleteTermReplacedByIterator.next();

				System.out.println("TermMergeHistoryItem: obsoletes section: " +
						"replacementTerm = " + replacementTerm +
						" slaveNode = " + slaveNode);

				//If the term that is being subsumed in the merge is mentioned in the collection
				// of replaced_by terms for the obsolete term currently being examined, then call this method. 
				if (slaveNode == replacementTerm){
					updateReplacedByOnMerge(out, obsoleteTerm, slaveNode, masterNode);
				}
			}

		}
	}
	/**
	 * <p>Used to change over the consider target on an obsolete term in a situation where the 
	 * current consider tag target has been subsumed in a merge. The masterNode is the term that subsumes
	 * the slaveNode in the merge. The obsoleteTerm is the obsolete node that has the consider term. The out
	 * List is a list of HistoryItems showing the edits being carried out. 
	 * </p><p>
	 * This method adds to 'out' two new HistoryItems. One of these removes the old consider tag that pointed
	 * to the term being subsumed in the merge, the other adds a new consider tag pointing to the 
	 * master term in the merge. 
	 * 
	 * @param obsoleteTerm The obsolete term that has the consider tag.
	 * @param out List of HistoryItems.
	 * @param slaveNode The term being subsumed.
	 * @param masterNode The term subsuming. 
	 * @return
	 */
	private List updateConsiderReplacementOnMerge(List out, ObsoletableObject obsoleteTerm,
			OBOClass slaveNode, OBOClass masterNode) {
		//System.out.println("TermmergeHistoryItem: updateConsiderReplacementOnMerge: object = " + obsoleteTerm +
		//" slaveNode = " + slaveNode + " masterNode = " + masterNode);
		out.add(new AddConsiderHistoryItem(obsoleteTerm, masterNode));
		out.add(new RemoveConsiderHistoryItem(obsoleteTerm, slaveNode));

		return out;
	}

	/**
	 * <p>Used to change over the replaced_by target on an obsolete term in a situation where the 
	 * current replaced_by tag target has been subsumed in a merge. The masterNode is the term that subsumes
	 * the slaveNode in the merge. The obsoleteTerm is the obsolete node that has the replaced_by term. The out
	 * List is a list of HistoryItems showing the edits being carried out. 
	 * </p><p>
	 * This method adds to 'out' two new HistoryItems. One of these removes the old replaced_by tag that pointed
	 * to the term being subsumed in the merge, the other adds a new replaced_by tag pointing to the 
	 * master term in the merge. 
	 * 
	 * @param obsoleteTerm The obsolete term that has the consider tag.
	 * @param out List of HistoryItems.
	 * @param slaveNode The term being subsumed.
	 * @param masterNode The term subsuming. 
	 * @return
	 */
	private List updateReplacedByOnMerge(List out, ObsoletableObject obsoleteTerm,
			OBOClass slaveNode, OBOClass masterNode) {
		//System.out.println("TermmergeHistoryItem: updateReplacedByOnMerge: object = " + obsoleteTerm +
		//" slaveNode = " + slaveNode + " masterNode = " + masterNode);
		out.add(new RemoveReplacementHistoryItem(obsoleteTerm, slaveNode));
		out.add(new AddReplacementHistoryItem(obsoleteTerm, masterNode));

		return out;
	}

	@Override
	public String getShortName() {
		return "merge";
	}

	public String getMaster() {
		return target;
	}

	public void setSlave(String slave) {
		this.slave = slave;
	}

	public String getSlave() {
		return slave;
	}

	@Override
	public String toString() {
		return "Merged " + slave + " into " + target;
	}

	/**
	 * Overridden to return both target and slave, as the stanzas of both terms will
	 * have been edited in this step. 
	 * 
	 * @return Set editedTerms
	 */
	@Override
	public Set getEditedTerms() {
		editedTerms.add(target);
		editedTerms.add(slave);
		return super.getEditedTerms();
	}

}
