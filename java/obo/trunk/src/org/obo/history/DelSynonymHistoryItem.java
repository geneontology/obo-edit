package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DelSynonymHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DelSynonymHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -7464356593326841552L;
	protected String synonym;

	public DelSynonymHistoryItem() {
	}

	public DelSynonymHistoryItem(SynonymedObject target, String synonym) {
		this(target.getID(), synonym);
	}

	public DelSynonymHistoryItem(String target, String synonym) {
		this.target = target;
		this.synonym = synonym;
	}

	public String getSynonym() {
		return synonym;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	@Override
	public String getShortName() {
		return "DEL_RELATED_SYNONYM";
	}

	@Override
	public String toString() {
		return "Deleted synonym \"" + synonym + "\" from " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
	
	public boolean equals(Object o) {
		if (!(o instanceof DelSynonymHistoryItem))
			return false;
		DelSynonymHistoryItem dshi = (DelSynonymHistoryItem) o;
		return dshi.getTarget().equals(target) && dshi.getSynonym().equals(synonym);
	}
}
