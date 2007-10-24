package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

public class CyclicHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7013371178175196469L;
	protected boolean oldCyclic;

	public CyclicHistoryItem(OBOProperty target) {
		this(target.getID(), target.isCyclic());
	}

	public CyclicHistoryItem() {
		this(null, false);
	}

	public CyclicHistoryItem(String target, boolean oldCyclic) {
		this.target = target;
		this.oldCyclic = oldCyclic;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(oldCyclic);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CyclicHistoryItem))
			return false;
		CyclicHistoryItem item = (CyclicHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& oldCyclic == item.getOldCyclic();
	}

	public void setOldCyclic(boolean oldCyclic) {
		this.oldCyclic = oldCyclic;
	}

	public boolean getOldCyclic() {
		return oldCyclic;
	}

	@Override
	public String getShortName() {
		return "changed is cyclic";
	}

	@Override
	public String toString() {
		return "changed \"is cyclic\" status of " + target + " to "
				+ (!oldCyclic);
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
