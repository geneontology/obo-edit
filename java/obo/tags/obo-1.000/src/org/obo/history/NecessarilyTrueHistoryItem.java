package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

public class NecessarilyTrueHistoryItem extends LinkHistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1365245811175806366L;
	protected boolean oldNecessary;

	public NecessarilyTrueHistoryItem() {
		this(null, false);
	}

	public NecessarilyTrueHistoryItem(StringRelationship rel,
			boolean oldNecessary) {
		this.rel = rel;
		this.oldNecessary = oldNecessary;
	}

	public NecessarilyTrueHistoryItem(OBORestriction tr) {
		this(new StringRelationship(tr), tr.isNecessarilyTrue());
	}

	@Override
	public int hashCode() {
		return getHash(rel) ^ getHash(oldNecessary);
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof NecessarilyTrueHistoryItem) {
			NecessarilyTrueHistoryItem item = (NecessarilyTrueHistoryItem) o;
			return ObjectUtil.equals(rel, item.getRel())
					&& oldNecessary == item.getOldNecessary();
		} else
			return false;
	}

	public void setOldNecessary(boolean oldNecessary) {
		this.oldNecessary = oldNecessary;
	}

	public boolean getOldNecessary() {
		return oldNecessary;
	}

	@Override
	public String getShortName() {
		return "changed necessarily true";
	}

	@Override
	public String toString() {
		return "changed necessarily true status of " + rel + " to "
				+ (!oldNecessary);
	}
}
