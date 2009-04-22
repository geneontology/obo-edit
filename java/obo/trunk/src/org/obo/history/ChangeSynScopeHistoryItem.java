package org.obo.history;

import java.util.Collection;
import java.util.Set;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class ChangeSynScopeHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ChangeSynScopeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -869201134791523102L;
	protected String synonym;
	protected int oldScope;
	protected int newScope;

	public ChangeSynScopeHistoryItem() {
	}

	public ChangeSynScopeHistoryItem(SynonymedObject target, Synonym synonym,
			int newScope) {
		this(target.getID(), synonym.getText(), synonym.getScope(), newScope);
	}

	public ChangeSynScopeHistoryItem(String target, String synonym,
			int oldScope, int newScope) {
		this.target = target;
		this.synonym = synonym;
		this.oldScope = oldScope;
		this.newScope = newScope;
	}

	public boolean equals(Object o) {
		if (!(o instanceof ChangeSynScopeHistoryItem))
			return false;
		ChangeSynScopeHistoryItem item = (ChangeSynScopeHistoryItem) o;
		return item.getTarget().equals(target)
				&& item.getSynonym().equals(synonym)
				&& item.getOldScope() == oldScope
				&& item.getNewScope() == newScope;
	}

	@Override
	public String toString() {
		return "Changed synonym scope of " + target + " from \""
				+ getScopeString(oldScope) + "\" to \""
				+ getScopeString(newScope) + "\"";
	}

	protected String getScopeString(int scope) {
		if (scope == Synonym.UNKNOWN_SCOPE)
			return "Unknown";
		else if (scope == Synonym.RELATED_SYNONYM)
			return "Related";
		else if (scope == Synonym.EXACT_SYNONYM)
			return "Exact";
		else if (scope == Synonym.NARROW_SYNONYM)
			return "Narrow";
		else if (scope == Synonym.BROAD_SYNONYM)
			return "Broad";
		else
			return "!!ILLEGAL!!";
	}

	public String getSynonym() {
		return synonym;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	public void setNewScope(int newScope) {
		this.newScope = newScope;
	}

	public int getNewScope() {
		return newScope;
	}

	public void setOldScope(int oldScope) {
		this.oldScope = oldScope;
	}

	public int getOldScope() {
		return oldScope;
	}

	@Override
	public String getShortName() {
		return "CHANGE_RELATED_SYNONYM_SCOPE";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
	
	@Override
	public Set getEditedNodes() {
	edited.add(target);
		return super.getEditedNodes();
	}
}
