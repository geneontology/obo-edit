package org.obo.reasoner.rbr;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.OperationModel;
import org.obo.history.OperationWarning;
import org.obo.history.StringRelationship;
import org.obo.history.TermMacroHistoryItem;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.HistoryUtil;

import org.apache.log4j.*;

public class ReasonerOperationModel implements OperationModel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerOperationModel.class);

	protected ReasonedLinkDatabase reasoner;

	protected OBOSession session;

	public ReasonerOperationModel(ReasonedLinkDatabase reasoner) {
		setReasoner(reasoner);
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}

	public void setSession(OBOSession history) {
		this.session = history;
	}

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}
	
	public void addLockstepModel(OperationModel model) {
	}
	
	public void removeLockstepModel(OperationModel model) {
	}

	public OperationWarning apply(HistoryItem item) {
		if (item instanceof CreateLinkHistoryItem) {
			CreateLinkHistoryItem tchi = (CreateLinkHistoryItem) item;
			StringRelationship sr = new StringRelationship(
					tchi.getTarget(), tchi.getTypeID(), tchi.getParentID());
			Link tr = HistoryUtil.createRealRel(session, sr);
			/*
			 * new OBORestrictionImpl((LinkedObject) history.
			 * getObject(sr.getChild()), (LinkedObject) history.
			 * getObject(tchi.getTarget()), (OBOProperty)
			 * history.getObject(sr.getType()));
			 */
			reasoner.addLink(tr);
		} else if (item instanceof DeleteLinkHistoryItem) {
			DeleteLinkHistoryItem dlhi = (DeleteLinkHistoryItem) item;
			StringRelationship sr = dlhi.getRel();

			Link link = HistoryUtil.createRealRel(session, sr);
			/*
			 * final Link link = new OBORestrictionImpl((LinkedObject) history.
			 * getObject(sr.getChild()), (LinkedObject) history.
			 * getObject(sr.getParent()), (OBOProperty)
			 * history.getObject(sr.getType()));
			 */
			reasoner.removeLink(link);
		} else if (item instanceof CompletesHistoryItem) {
			CompletesHistoryItem chi = (CompletesHistoryItem) item;
			Link link = HistoryUtil.getRealRel(session, chi.getRel());
			setCompleteness(link, !chi.getOldCompletes());
		}
		return null;
	}

	public OperationWarning reverse(HistoryItem item) {
		if (item instanceof CreateLinkHistoryItem) {
			CreateLinkHistoryItem tchi = (CreateLinkHistoryItem) item;
			StringRelationship sr = new StringRelationship(
					tchi.getTarget(), tchi.getTypeID(), tchi.getParentID());
			Link link = HistoryUtil.createRealRel(session, sr);
			/*
			 * final Link link = new OBORestrictionImpl((LinkedObject) history.
			 * getObject(sr.getChild()), (LinkedObject) history.
			 * getObject(tchi.getTarget()), (OBOProperty)
			 * history.getObject(sr.getType()));
			 */
			reasoner.removeLink(link);
		} else if (item instanceof DeleteLinkHistoryItem) {
			DeleteLinkHistoryItem dlhi = (DeleteLinkHistoryItem) item;
			StringRelationship sr = dlhi.getRel();

			Link link = HistoryUtil.createRealRel(session, sr);
			/*
			 * final Link link = new OBORestrictionImpl((LinkedObject) history.
			 * getObject(sr.getChild()), (LinkedObject) history.
			 * getObject(sr.getParent()), (OBOProperty)
			 * history.getObject(sr.getType()));
			 */
			reasoner.addLink(link);
		} else if (item instanceof CompletesHistoryItem) {
			CompletesHistoryItem chi = (CompletesHistoryItem) item;
			Link link = HistoryUtil.getRealRel(session, chi.getRel());
			setCompleteness(link, chi.getOldCompletes());
		}
		return null;
	}

	protected void setCompleteness(Link link, boolean completes) {
		reasoner.removeLink(new OBORestrictionImpl(link, !completes));
		reasoner.addLink(new OBORestrictionImpl(link, completes));
	}
}
