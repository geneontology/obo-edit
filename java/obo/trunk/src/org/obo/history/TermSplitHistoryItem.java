package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class TermSplitHistoryItem extends SubclassedMacroHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermSplitHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -3983637863915469299L;
	protected String result;
	protected boolean addType;

	public TermSplitHistoryItem() {
		this((String) null, null, false);
	}

	public TermSplitHistoryItem(OBOClass target, String result, boolean addType) {
		this(target.getID(), result, addType);
	}

	public TermSplitHistoryItem(String target, String result, boolean addType) {
		super("split");
		setTarget(target);
		this.result = result;
		this.addType = addType;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(result) ^ getHash(addType);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermSplitHistoryItem))
			return false;
		TermSplitHistoryItem item = (TermSplitHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(result, item.getResult())
				&& addType == item.addType();
	}

	@Override
	public void setResult(String result) {
		this.result = result;
	}

	public void setAddType(boolean addType) {
		this.addType = addType;
	}

	public boolean addType() {
		return addType;
	}

	@Override
	public void addItem(HistoryItem item) {
	}

	@Override
	public void removeItem(HistoryItem item) {
	}

	@Override
	public void setHistoryItems(Vector v) {
	}

	@Override
	public OperationWarning lock(OBOSession history) {
		historyItems = new Vector();
		OperationWarning ow = getItems(history, historyItems);
		OperationWarning ow2 = super.lock(history);
		if (ow == null) {
			return ow2;
		} else {
			if (ow2 != null)
				ow.addWarning(ow2);
			return ow;
		}
	}

	@Override
	protected OperationWarning getItems(OBOSession history, List out) {
		OBOClass targetClass = (OBOClass) history.getObject(target);
		if (targetClass == null)
			return new OperationWarning("Could not split with unrecognized "
					+ "target " + targetClass);

		out.add(new CreateObjectHistoryItem(result,
				(addType ? OBOClass.OBO_PROPERTY.getID() : OBOClass.OBO_CLASS
						.getID())));

		Iterator it = targetClass.getParents().iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();

			LinkedObject parent = tr.getParent();
			out.add(new CreateLinkHistoryItem(result, tr
					.getType().getID(), parent.getID()));
		}

		it = targetClass.getChildren().iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			LinkedObject child = tr.getChild();
			out.add(new CreateLinkHistoryItem(child.getID(), tr.getType()
					.getID(), result));
		}

		out.add(new NameChangeHistoryItem("", targetClass.getName(), result));

		it = targetClass.getDbxrefs().iterator();
		while (it.hasNext()) {
			Dbxref ref = (Dbxref) it.next();
			out.add(new AddDbxrefHistoryItem(result, ref, false, null));
		}

		it = targetClass.getSynonyms().iterator();
		while (it.hasNext()) {
			Synonym syn = (Synonym) it.next();

			out.add(new AddSynonymHistoryItem(result, syn.getText()));

			if (syn.getSynonymCategory() != null)
				out.add(new ChangeSynCategoryHistoryItem(result, syn.getText(),
						null, syn.getSynonymCategory().getID()));

			if (syn.getScope() == Synonym.RELATED_SYNONYM)
				out.add(new ChangeSynScopeHistoryItem(result, syn.getText(),
						Synonym.RELATED_SYNONYM, syn.getScope()));

			Iterator it2 = syn.getDbxrefs().iterator();
			while (it2.hasNext()) {
				Dbxref ref = (Dbxref) it2.next();
				out.add(new AddDbxrefHistoryItem(result, ref, false, syn
						.getText()));
			}
		}

		/*
		 * 
		 * it = target.getSynonyms().iterator(); while(it.hasNext()) { Synonym
		 * syn = (Synonym) it.next(); Vector refchanges = new Vector(); Iterator
		 * it2 = syn.getDbxrefs().iterator(); while(it2.hasNext()) { Dbxref ref =
		 * (Dbxref) it2.next(); TermTextHistoryItem.DbxrefEdit refedit = new
		 * TermTextHistoryItem.DbxrefEdit(null, ref, false, true, result);
		 * refchanges.add(refedit); } TermTextHistoryItem.SynonymEdit edit = new
		 * TermTextHistoryItem. SynonymEdit(null, syn, false, true, refchanges,
		 * result); changes.add(edit); }
		 */
		return null;
	}

	@Override
	public String getShortName() {
		return "split";
	}

	@Override
	public String getResult() {
		return result;
	}

	@Override
	public String toString() {
		return "Split " + target + " to create " + result;
	}
}
