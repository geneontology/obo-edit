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

public class SynonymCategoryHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymCategoryHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 2764272842685200832L;
	protected SynonymCategory oldcat;
	protected SynonymCategory newcat;
	protected boolean isAdd;
	protected boolean isDel;

	public SynonymCategoryHistoryItem() {
		this(null, null, false, false);
	}

	public SynonymCategoryHistoryItem(SynonymCategory oldcat,
			SynonymCategory newcat, boolean isAdd, boolean isDel) {
		if (oldcat != null)
			this.oldcat = (SynonymCategory) oldcat.clone();
		if (newcat != null)
			this.newcat = (SynonymCategory) newcat.clone();

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
		if (!(o instanceof SynonymCategoryHistoryItem))
			return false;
		SynonymCategoryHistoryItem item = (SynonymCategoryHistoryItem) o;
		return ObjectUtil.equals(oldcat, item.getOldCategory())
				&& ObjectUtil.equals(newcat, item.getNewCategory())
				&& isAdd == item.isAdd() && isDel == item.isDel();
	}

	public void setOldCat(SynonymCategory oldcat) {
		this.oldcat = oldcat;
	}

	public void setNewCat(SynonymCategory newcat) {
		this.newcat = newcat;
	}

	public void setIsAdd(boolean isAdd) {
		this.isAdd = isAdd;
	}

	public void setIsDel(boolean isDel) {
		this.isDel = isDel;
	}

	public SynonymCategory getOldCategory() {
		return oldcat;
	}

	public SynonymCategory getNewCategory() {
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
		return "Synonym Category Edit";
	}

	@Override
	public String toString() {
		if (isAdd)
			return "Created synonym category " + newcat;
		else if (isDel)
			return "Removed synonym category " + oldcat;
		else
			return "Changed synonym category " + oldcat + " to " + newcat;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
