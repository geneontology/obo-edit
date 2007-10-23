package org.obo.history;

import org.obo.datamodel.*;

import java.util.*;

public class InverseNecHistoryItem extends LinkHistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6416733775998000689L;
	protected boolean oldInverseNec;

	public InverseNecHistoryItem() {
		this(null, false);
	}

	public InverseNecHistoryItem(StringRelationship rel, boolean oldInverseNec) {
		this.oldInverseNec = oldInverseNec;
	}

	public InverseNecHistoryItem(OBORestriction tr) {
		this(new StringRelationship(tr), tr.isInverseNecessarilyTrue());
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof InverseNecHistoryItem))
			return false;
		InverseNecHistoryItem item = (InverseNecHistoryItem) o;
		return super.equals(item.getRel())
				&& oldInverseNec == item.getOldInverseNecessary();
	}

	@Override
	public int hashCode() {
		return super.hashCode() ^ getHash(oldInverseNec);
	}

	public void setOldInverseNec(boolean oldInverseNec) {
		this.oldInverseNec = oldInverseNec;
	}

	public boolean getOldInverseNecessary() {
		return oldInverseNec;
	}

	@Override
	public String getShortName() {
		return "changed inverse necessarily true";
	}

	@Override
	public String toString() {
		return "changed inverse necessarily true status of " + rel + " to "
				+ (!oldInverseNec);
	}
}
