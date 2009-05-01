package org.obo.history;

import java.util.Set;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DeleteLinkHistoryItem extends LinkHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DeleteLinkHistoryItem.class);
	String childID;
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
	public Set getEditedNodes() {
		edited.add(rel.getChild());
		return (Set) edited.clone();
	}
	
	@Override
	public String toString() {
		return "Deleted " + rel;
	}

	@Override
	public String getShortName() {
		return "delete";
	}
	
	@Override
	public Set getEditedTerms() {
		editedTerms.add(rel.getChild());
		System.out.println("DeleteLinkHistoryItem: getEditedTerms: rel.getChild() = " + rel.getChild());
		return (Set) editedTerms.clone();
	}
}
