package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

public class CreateObjectHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2256842572810288045L;

	protected String objectID;

	protected String typeID;

	protected boolean anonymous;

	public CreateObjectHistoryItem() {
	}

	public CreateObjectHistoryItem(String objectID, String typeID) {
		this(objectID, false, typeID);
	}

	public CreateObjectHistoryItem(String objectID, boolean anonymous,
			String typeID) {
		this.objectID = objectID;
		this.anonymous = anonymous;
		this.typeID = typeID;
	}

	public boolean equals(Object o) {
		if (!(o instanceof CreateObjectHistoryItem))
			return false;
		CreateObjectHistoryItem item = (CreateObjectHistoryItem) o;
		return item.getObjectID().equals(objectID)
				&& item.getObjectType().equals(typeID)
				&& item.isAnonymous() == anonymous;
	}

	public void setObjectID(String objectID) {
		this.objectID = objectID;
	}

	public void setTypeID(String typeID) {
		this.typeID = typeID;
	}

	public void setIsAnonymous(boolean anonymous) {
		this.anonymous = anonymous;
	}

	public boolean isAnonymous() {
		return anonymous;
	}

	public String getObjectID() {
		return objectID;
	}

	public String getObjectType() {
		return typeID;
	}

	@Override
	public String getShortName() {
		return "Create object";
	}

	@Override
	public String toString() {
		return "Created " + (anonymous ? "anonymous " : "") + "object of type "
				+ typeID + " with id " + objectID;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		// do nothing; if there's a valid forwarding, we've got an id clash
		// anyway
		return null;
	}
}
