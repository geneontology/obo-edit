package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class SecondaryIDHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SecondaryIDHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 841205323715737978L;
	protected String secondary_id;
	protected boolean delete;

	public SecondaryIDHistoryItem() {
		this((String) null, null, false);
	}

	public SecondaryIDHistoryItem(MultiIDObject target, String secondary_id,
			boolean delete) {
		this(target.getID(), secondary_id, delete);
	}

	public SecondaryIDHistoryItem(String target, String secondary_id,
			boolean delete) {
		this.target = target;
		this.secondary_id = secondary_id;
		this.delete = delete;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(secondary_id) ^ getHash(delete);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof SecondaryIDHistoryItem))
			return false;
		SecondaryIDHistoryItem item = (SecondaryIDHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(secondary_id, item.getSecondaryID())
				&& delete == item.isDelete();
	}

	public void setDelete(boolean delete) {
		this.delete = delete;
	}

	public void setSecondaryID(String secondary_id) {
		this.secondary_id = secondary_id;
	}

	public boolean isDelete() {
		return delete;
	}

	public String getSecondaryID() {
		return secondary_id;
	}

	@Override
	public String getShortName() {
		return "add secondary id";
	}

	@Override
	public String toString() {
		return "added secondary id " + secondary_id + " to " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
}
