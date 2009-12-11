package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;


import org.apache.log4j.*;

public class CreateLinkHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CreateLinkHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -3796745679067017708L;

	protected String typeID;

	protected String parentID;

	public CreateLinkHistoryItem() {
	}
	
	public CreateLinkHistoryItem(Link link) {
		this(link.getChild(), link.getType(), link.getParent());
	}

	public CreateLinkHistoryItem(String child, String type, String parent) {
		this.target = child;
		this.typeID = type;
		this.parentID = parent;
	}

	public CreateLinkHistoryItem(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		this(child.getID(), type.getID(), parent.getID());
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CreateLinkHistoryItem))
			return false;
		CreateLinkHistoryItem item = (CreateLinkHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(typeID, item.getTypeID())
				&& ObjectUtil.equals(parentID, item.getParentID());
	}

	@Override
	public String getShortName() {
		return "created link";
	}

	@Override
	public String toString() {
		return "Created link " + target + " -" + typeID + "-> " + parentID;
	}

	public String getParentID() {
		return parentID;
	}

	public String getTypeID() {
		return typeID;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		if (ObjectUtil.equals(target, oldID)
				|| ObjectUtil.equals(typeID, oldID)
				|| ObjectUtil.equals(parentID, oldID)) {
			HistoryList out = new DefaultHistoryList();
			Iterator it = newIDs.iterator();
			while (it.hasNext()) {
				String id = it.next().toString();
				String newChildID = target;
				String newTypeID = typeID;
				String newParentID = parentID;
				if (ObjectUtil.equals(newChildID, oldID))
					newChildID = id;
				if (ObjectUtil.equals(newTypeID, oldID))
					newTypeID = id;
				if (ObjectUtil.equals(newParentID, oldID))
					newParentID = id;
				out.addItem(new CreateLinkHistoryItem(newChildID, newTypeID,
						newParentID));
			}
			return out;
		}
		return null;
	}

	public void setParentID(String parentID) {
		this.parentID = parentID;
	}

	public void setTypeID(String typeID) {
		this.typeID = typeID;
	}
}
