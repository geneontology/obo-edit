package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;


import org.apache.log4j.*;

public class TermSubsetHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermSubsetHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -2799032354303157650L;
	protected TermSubset oldcat;
	protected TermSubset newsubset;
	protected boolean isAdd;
	protected boolean isDel;

	public TermSubsetHistoryItem() {
		this(null, null, false, false);
	}

	public TermSubsetHistoryItem(TermSubset oldsubset, TermSubset newsubset,
			boolean isAdd, boolean isDel) {
		if (oldsubset != null)
			this.oldcat = (TermSubset) oldsubset.clone();
		if (newsubset != null)
			this.newsubset = (TermSubset) newsubset.clone();

		this.isAdd = isAdd;
		this.isDel = isDel;
	}

	@Override
	public int hashCode() {
		return getHash(oldcat) ^ getHash(newsubset) ^ getHash(isAdd)
				^ getHash(isDel);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermSubsetHistoryItem))
			return false;
		TermSubsetHistoryItem item = (TermSubsetHistoryItem) o;
		return ObjectUtil.equals(oldcat, item.getOldCategory())
				&& ObjectUtil.equals(newsubset, item.getNewSubset())
				&& isAdd == item.isAdd() && isDel == item.isDel();
	}

	public void setOldCat(TermSubset oldcat) {
		this.oldcat = oldcat;
	}

	public void setNewCat(TermSubset newcat) {
		this.newsubset = newcat;
	}

	public void setIsAdd(boolean isAdd) {
		this.isAdd = isAdd;
	}

	public void setIsDel(boolean isDel) {
		this.isDel = isDel;
	}

	public TermSubset getOldCategory() {
		return oldcat;
	}

	public TermSubset getNewSubset() {
		return newsubset;
	}

	public boolean isAdd() {
		return isAdd;
	}

	public boolean isDel() {
		return isDel;
	}

	@Override
	public String getShortName() {
		return "Subset Edit";
	}

	@Override
	public String toString() {
		if (isAdd)
			return "Created category " + newsubset;
		else if (isDel)
			return "Removed category " + oldcat;
		else
			return "Changed category " + oldcat + " to " + newsubset;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		// TODO Auto-generated method stub
		return null;
	}
}
