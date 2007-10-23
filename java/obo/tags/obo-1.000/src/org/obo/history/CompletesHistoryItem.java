package org.obo.history;

import org.obo.datamodel.*;

import java.util.*;

public class CompletesHistoryItem extends LinkHistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4418549899358070821L;
	protected boolean oldCompletes;

	public CompletesHistoryItem() {
		this(null, false);
	}

	public CompletesHistoryItem(String child, String type, String parent,
			boolean oldCompletes) {
		this(new StringRelationship(child, type, parent), oldCompletes);
	}

	public CompletesHistoryItem(StringRelationship rel, boolean oldCompletes) {
		setRel(rel);
		this.oldCompletes = oldCompletes;
	}

	public CompletesHistoryItem(OBORestriction tr) {
		this(new StringRelationship(tr), tr.completes());
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CompletesHistoryItem))
			return false;
		return super.equals(o) &&
			oldCompletes == ((CompletesHistoryItem) o).getOldCompletes();
	}

	@Override
	public int hashCode() {
		 return super.hashCode() ^ getHash(oldCompletes);
	}

	public void setOldCompletes(boolean oldCompletes) {
		this.oldCompletes = oldCompletes;
	}

	public boolean getOldCompletes() {
		return oldCompletes;
	}

	@Override
	public String getShortName() {
		return "changed completes";
	}

	@Override
	public String toString() {
		return "changed \"completes\" status of " + rel + " to "
				+ (!oldCompletes);
	}
}
