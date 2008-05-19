package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;


import org.apache.log4j.*;

public class TermNamespaceHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermNamespaceHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4109669422414877334L;
	protected String oldnsid;
	protected String newnsid;
	protected boolean isAdd;
	protected boolean isDel;

	public TermNamespaceHistoryItem(String oldnsid, String newnsid,
			boolean isAdd, boolean isDel) {
		this.oldnsid = oldnsid;
		this.newnsid = newnsid;

		this.isAdd = isAdd;
		this.isDel = isDel;
	}

	public TermNamespaceHistoryItem() {
		this(null, null, false, false);
	}

	@Override
	public int hashCode() {
		return getHash(oldnsid) ^ getHash(newnsid) ^ getHash(isAdd)
				^ getHash(isDel);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermNamespaceHistoryItem))
			return false;
		TermNamespaceHistoryItem item = (TermNamespaceHistoryItem) o;
		return ObjectUtil.equals(oldnsid, item.getOldID())
				&& ObjectUtil.equals(newnsid, item.getNewID())
				&& isAdd == item.isAdd() && isDel == item.isDel();
	}

	public void setIsAdd(boolean isAdd) {
		this.isAdd = isAdd;
	}

	public void setIsDel(boolean isDel) {
		this.isDel = isDel;
	}

	public void setOldID(String id) {
		this.oldnsid = id;
	}

	public void setNewID(String id) {
		this.newnsid = id;
	}

	public String getOldID() {
		return oldnsid;
	}

	public String getNewID() {
		return newnsid;
	}

	public boolean isAdd() {
		return isAdd;
	}

	public boolean isDel() {
		return isDel;
	}

	@Override
	public String getShortName() {
		return "Namespace Edit";
	}

	@Override
	public String toString() {
		if (isAdd)
			return "Created namespace " + newnsid;
		else if (isDel)
			return "Removed namespace " + oldnsid;
		else
			return "Changed namespace " + oldnsid + " to " + newnsid;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
