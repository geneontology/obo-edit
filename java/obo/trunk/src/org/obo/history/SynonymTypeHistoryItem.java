package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;


/**
 * 
 * Describes changes to a synonym category
 * 
 */

import org.apache.log4j.*;

public class SynonymTypeHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymTypeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 2764272842685200832L;
	protected SynonymType oldcat;
	protected SynonymType newtype;
	protected boolean isAdd;
	protected boolean isDel;

	public SynonymTypeHistoryItem() {
		this(null, null, false, false);
	}

	public SynonymTypeHistoryItem(SynonymType oldtype,
			SynonymType newcat, boolean isAdd, boolean isDel) {
		if (oldtype != null)
			this.oldcat = (SynonymType) oldtype.clone();
		if (newcat != null)
			this.newtype = (SynonymType) newcat.clone();

		this.isAdd = isAdd;
		this.isDel = isDel;
	}

	@Override
	public int hashCode() {
		return getHash(oldcat) ^ getHash(newtype) ^ getHash(isAdd)
				^ getHash(isDel);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof SynonymTypeHistoryItem))
			return false;
		SynonymTypeHistoryItem item = (SynonymTypeHistoryItem) o;
		return ObjectUtil.equals(oldcat, item.getOldCategory())
				&& ObjectUtil.equals(newtype, item.getNewCategory())
				&& isAdd == item.isAdd() && isDel == item.isDel();
	}

	public void setOldCat(SynonymType oldcat) {
		this.oldcat = oldcat;
	}

	public void setNewCat(SynonymType newcat) {
		this.newtype = newcat;
	}

	public void setIsAdd(boolean isAdd) {
		this.isAdd = isAdd;
	}

	public void setIsDel(boolean isDel) {
		this.isDel = isDel;
	}

	public SynonymType getOldCategory() {
		return oldcat;
	}

	public SynonymType getNewCategory() {
		return newtype;
	}

	public boolean isAdd() {
		return isAdd;
	}

	public boolean isDel() {
		return isDel;
	}

	@Override
	public String getShortName() {
		return "Synonym Type Edit";
	}

	@Override
	public String toString() {
		if (isAdd)
			return "Created synonym category " + newtype;
		else if (isDel)
			return "Removed synonym category " + oldcat;
		else
			return "Changed synonym category " + oldcat + " to " + newtype;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
