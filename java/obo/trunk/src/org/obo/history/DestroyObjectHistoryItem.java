package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

public class DestroyObjectHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1065120801254230974L;
	protected IdentifiedObject object;

	public DestroyObjectHistoryItem() {
	}

	public DestroyObjectHistoryItem(IdentifiedObject object) {
		this.object = object;
		setTarget(object.getID());
	}
	
	public IdentifiedObject getObject() {
		return object;
	}

	@Override
	public String getShortName() {
		return "destroy";
	}

	@Override
	public String toString() {
		return "Destroyed object " + object.getID();
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
	
	public boolean equals(Object o) {
		if (!(o instanceof DestroyObjectHistoryItem))
			return false;
		return object.equals(((DestroyObjectHistoryItem) o).getObject());
	}
}
