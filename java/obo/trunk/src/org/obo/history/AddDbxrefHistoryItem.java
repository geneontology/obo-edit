package org.obo.history;

import java.util.Collection;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class AddDbxrefHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddDbxrefHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4527059742586142371L;
	protected Dbxref newref;
	protected boolean isDef;
	protected String synonym;

	public AddDbxrefHistoryItem() {
		this(null, null, false, null);
	}

	public AddDbxrefHistoryItem(String target, Dbxref newref, boolean isDef,
			String synonym) {
		this.newref = newref;
		this.isDef = isDef;
		this.synonym = synonym;
		this.target = target;
	}

	public boolean equals(Object o) {
		if (!(o instanceof AddDbxrefHistoryItem))
			return false;
		AddDbxrefHistoryItem item = (AddDbxrefHistoryItem) o;
		return ObjectUtil.equals(item.getDbxref(), newref)
				&& item.isDef() == isDef
				&& ObjectUtil.equals(item.getSynonym(), synonym)
				&& ObjectUtil.equals(item.getTarget(), target);
	}

	@Override
	public String toString() {
		return "Added " + (isDef ? "definition" : "general") + " dbxref "
				+ newref + " to "
				+ (synonym != null ? "synonym " + synonym + " of " : "")
				+ target;
	}

	public Dbxref getDbxref() {
		return newref;
	}

	public boolean isDef() {
		return isDef;
	}

	public String getSynonym() {
		return synonym;
	}

	public void setDbxref(Dbxref newref) {
		this.newref = newref;
	}

	public void setDef(boolean isDef) {
		this.isDef = isDef;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	@Override
	public String getShortName() {
		return "ADD_DBXREF";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
}
