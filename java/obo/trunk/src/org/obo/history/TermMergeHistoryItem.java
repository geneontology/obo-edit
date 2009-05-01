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
		out
				.add(new SecondaryIDHistoryItem(masterNode, slaveNode.getID(),
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
