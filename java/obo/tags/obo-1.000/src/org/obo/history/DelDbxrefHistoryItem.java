package org.obo.history;

import java.util.Collection;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

public class DelDbxrefHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3530001915193918269L;
	protected Dbxref newref;
	protected boolean isDef;
	protected String synonym;

	public DelDbxrefHistoryItem() {
	}

	public DelDbxrefHistoryItem(String target, Dbxref newref, boolean isDef,
			String synonym) {
		this.newref = newref;
		this.isDef = isDef;
		this.synonym = synonym;
		this.target = target;
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
		return "DEL_DBXREF";
	}

	@Override
	public String toString() {
		return "Deleted " + (isDef ? "definition" : "general") + " dbxref "
				+ newref + " from "
				+ (synonym != null ? "synonym " + synonym + " of " : "")
				+ target;
	}
	
	public boolean equals(Object o) {
		if (!(o instanceof DelDbxrefHistoryItem))
			return false;
		DelDbxrefHistoryItem item = (DelDbxrefHistoryItem) o;
		return item.getTarget().equals(target)
				&& item.getDbxref().equals(newref) && item.isDef() == isDef
				&& ObjectUtil.equals(item.getSynonym(), synonym);
	}
	
	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
