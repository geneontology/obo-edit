package org.obo.history;

import org.obo.datamodel.*;

public class DeleteLinkHistoryItem extends LinkHistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6351179262076970541L;

	public DeleteLinkHistoryItem() {
	}

	public DeleteLinkHistoryItem(Link tr) {
		this(createStringRelationship(tr));
	}

	public DeleteLinkHistoryItem(StringRelationship rel) {
		this.rel = rel;
	}
	
	public DeleteLinkHistoryItem(String childID, String typeID, String parentID) {
		this.rel = new StringRelationship(childID, typeID, parentID, false, false, false, null, null, null, null);
	}

	@Override
	public String toString() {
		return "Deleted " + rel;
	}

	@Override
	public String getShortName() {
		return "delete";
	}
}
