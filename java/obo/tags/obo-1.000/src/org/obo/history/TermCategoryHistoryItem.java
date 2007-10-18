package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;


public class TermCategoryHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2799032354303157650L;
	protected TermCategory oldcat;
	protected TermCategory newcat;
	protected boolean isAdd;
	protected boolean isDel;

	public TermCategoryHistoryItem() {
		this(null, null, false, false);
	}

	public TermCategoryHistoryItem(TermCategory oldcat, TermCategory newcat,
			boolean isAdd, boolean isDel) {
		if (oldcat != null)
			this.oldcat = (TermCategory) oldcat.clone();
		if (newcat != null)
			this.newcat = (TermCategory) newcat.clone();

		this.isAdd = isAdd;
		this.isDel = isDel;
	}

	@Override
	public int hashCode() {
		return getHash(oldcat) ^ getHash(newcat) ^ getHash(isAdd)
				^ getHash(isDel);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermCategoryHistoryItem))
			return false;
		TermCategoryHistoryItem item = (TermCategoryHistoryItem) o;
		return ObjectUtil.equals(oldcat, item.getOldCategory())
				&& ObjectUtil.equals(newcat, item.getNewCategory())
				&& isAdd == item.isAdd() && isDel == item.isDel();
	}

	public void setOldCat(TermCategory oldcat) {
		this.oldcat = oldcat;
	}

	public void setNewCat(TermCategory newcat) {
		this.newcat = newcat;
	}

	public void setIsAdd(boolean isAdd) {
		this.isAdd = isAdd;
	}

	public void setIsDel(boolean isDel) {
		this.isDel = isDel;
	}

	public TermCategory getOldCategory() {
		return oldcat;
	}

	public TermCategory getNewCategory() {
		return newcat;
	}

	public boolean isAdd() {
		return isAdd;
	}

	public boolean isDel() {
		return isDel;
	}

	@Override
	public String getShortName() {
		return "Category Edit";
	}

	@Override
	public String toString() {
		if (isAdd)
			return "Created category " + newcat;
		else if (isDel)
			return "Removed category " + oldcat;
		else
			return "Changed category " + oldcat + " to " + newcat;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		// TODO Auto-generated method stub
		return null;
	}
}
