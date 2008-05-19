package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class ChangeSynCategoryHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ChangeSynCategoryHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4593504540599957099L;

	protected String oldcat;

	protected String newcat;

	protected String synonym;

	public ChangeSynCategoryHistoryItem(IdentifiedObject io, Synonym synonym,
			SynonymCategory cat) {
		this(io.getID(), synonym.getText(),
				(synonym.getSynonymCategory() == null ? null : synonym
						.getSynonymCategory().getID()), (cat == null ? null
						: cat.getID()));
	}

	public ChangeSynCategoryHistoryItem() {
	}

	public ChangeSynCategoryHistoryItem(String target, String synonym,
			String oldcat, String newcat) {
		this.target = target;
		this.synonym = synonym;
		this.oldcat = oldcat;
		this.newcat = newcat;
	}

	public boolean equals(Object o) {
		if (!(o instanceof ChangeSynCategoryHistoryItem))
			return false;
		ChangeSynCategoryHistoryItem item = (ChangeSynCategoryHistoryItem) o;
		return item.getTarget().equals(target)
				&& item.getSynonym().equals(synonym)
				&& item.getOldCategory().equals(oldcat)
				&& item.getNewCategory().equals(newcat);
	}

	public void setOldCategory(String oldcat) {
		this.oldcat = oldcat;
	}

	public void setNewCategory(String newcat) {
		this.newcat = newcat;
	}

	public void setSynonym(String synonym) {
		this.synonym = synonym;
	}

	public String getSynonym() {
		return synonym;
	}

	public String getOldCategory() {
		return oldcat;
	}

	public String getNewCategory() {
		return newcat;
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
