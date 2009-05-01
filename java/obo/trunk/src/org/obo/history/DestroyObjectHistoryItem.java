package org.obo.history;

import java.util.Collection;
import java.util.Set;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DestroyObjectHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DestroyObjectHistoryItem.class);

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
	
	/**
 	 * Adds return value of object.getID() to editedTerms, rather than
 	 * contents of the "target" variable. 
	 *
	 * @return Set editedTerms
	 */
	@Override
	public Set getEditedTerms() {
		editedTerms.add(object.getID());
		return super.getEditedTerms();
	}
}
