package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class ChangeSynTypeHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ChangeSynTypeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4593504540599957099L;

	protected String oldtype;

	protected String newtype;

	protected String synonym;

	public ChangeSynTypeHistoryItem(IdentifiedObject io, Synonym synonym,
			SynonymType type) {
		this(io.getID(), synonym.getText(),
				(synonym.getSynonymType() == null ? null : synonym
						.getSynonymType().getID()), (type == null ? null
						: type.getID()));
	}

	public ChangeSynTypeHistoryItem() {
	}

	public ChangeSynTypeHistoryItem(String target, String synonym,
			String oldcat, String newcat) {
		this.target = target;
		this.synonym = synonym;
		this.oldtype = oldcat;
		this.newtype = newcat;
	}

	public boolean equals(Object o) {
		if (!(o instanceof ChangeSynTypeHistoryItem))
			return false;
		ChangeSynTypeHistoryItem item = (ChangeSynTypeHistoryItem) o;
		return item.getTarget().equals(target)
				&& item.getSynonym().equals(synonym)
				&& item.getOldType().equals(oldtype)
				&& item.getNewType().equals(newtype);
	}

	public void setOldType(String oldtype) {
		this.oldtype = oldtype;
	}

	public void setNewType(String newtype) {
		this.newtype = newtype;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	public String getSynonym() {
		return synonym;
	}

	public String getOldType() {
		return oldtype;
	}

	public String getNewType() {
		return newtype;
	}

	@Override
	public String getShortName() {
		return "CHANGE_RELATED_SYNONYM_CATEGORY";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
}
