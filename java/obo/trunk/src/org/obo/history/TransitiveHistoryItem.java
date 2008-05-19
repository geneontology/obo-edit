package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class TransitiveHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransitiveHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1657258768043000500L;
	protected boolean oldTransitive;

	public TransitiveHistoryItem() {
		this(null, false);
	}

	public TransitiveHistoryItem(OBOProperty target) {
		this(target.getID(), target.isTransitive());
	}

	public TransitiveHistoryItem(String target, boolean oldTransitive) {
		this.target = target;
		this.oldTransitive = oldTransitive;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(oldTransitive);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TransitiveHistoryItem))
			return false;
		TransitiveHistoryItem item = (TransitiveHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& oldTransitive == item.getOldTransitive();
	}

	public void setOldTransitive(boolean oldTransitive) {
		this.oldTransitive = oldTransitive;
	}

	public boolean getOldTransitive() {
		return oldTransitive;
	}

	@Override
	public String getShortName() {
		return "changed is transitive";
	}

	@Override
	public String toString() {
		return "changed \"is transitive\" status of " + target + " to "
				+ (!oldTransitive);
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
