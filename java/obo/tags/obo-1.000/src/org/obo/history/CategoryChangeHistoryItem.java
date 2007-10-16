package org.obo.history;

import java.util.Collection;

import org.bbop.util.*;
import org.obo.datamodel.*;


public class CategoryChangeHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4298761033710960309L;
	String category;
	boolean isDel;
	protected String target;

	public CategoryChangeHistoryItem() {
		this(null, false, null);
	}

	public CategoryChangeHistoryItem(String category, boolean isDel,
			String target) {
		this.category = category;
		this.isDel = isDel;
		this.target = target;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(category) ^ getHash(isDel);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CategoryChangeHistoryItem))
			return false;
		CategoryChangeHistoryItem edit = (CategoryChangeHistoryItem) o;
		return ObjectUtil.equals(target, edit.getTarget())
				&& ObjectUtil.equals(category, edit.getCategory())
				&& isDel == edit.isDel();
	}

	public void setIsDel(boolean isDel) {
		this.isDel = isDel;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	@Override
	public String getTarget() {
		return target;
	}

	@Override
	public void setTarget(String target) {
		this.target = target;
	}

	public boolean isDel() {
		return isDel;
	}

	public String getCategory() {
		return category;
	}

	@Override
	public String getShortName() {
		return "CATEGORY_EDIT";
	}

	@Override
	public String toString() {
		if (isDel)
			return "Removed " + target + " from category " + category;
		else
			return "Added " + target + " to category " + category;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
}
