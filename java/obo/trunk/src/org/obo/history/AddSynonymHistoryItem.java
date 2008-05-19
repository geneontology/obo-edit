package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class AddSynonymHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddSynonymHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1379733103321313741L;

	protected String synonym;

	public AddSynonymHistoryItem() {
	}

	public AddSynonymHistoryItem(SynonymedObject target, String synonym) {
		this(target.getID(), synonym);
	}

	public AddSynonymHistoryItem(String target, String synonym) {
		this.target = target;
		this.synonym = synonym;
	}

	public boolean equals(Object o) {
		if (!(o instanceof AddSynonymHistoryItem))
			return false;
		AddSynonymHistoryItem item = (AddSynonymHistoryItem) o;
		return item.getTarget().equals(target)
				&& item.getSynonym().equals(synonym);
	}

	public String getSynonym() {
		return synonym;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	@Override
	public String getShortName() {
		return "ADD_RELATED_SYNONYM";
	}

	@Override
	public String toString() {
		return "Added synonym \"" + synonym + "\" to " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
}
