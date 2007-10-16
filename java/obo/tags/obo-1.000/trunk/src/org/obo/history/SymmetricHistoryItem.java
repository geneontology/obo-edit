package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

public class SymmetricHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6478990619822580131L;
	protected boolean oldSymmetric;

	public SymmetricHistoryItem() {
		this(null, false);
	}

	public SymmetricHistoryItem(OBOProperty target) {
		this(target.getID(), target.isSymmetric());
	}

	public SymmetricHistoryItem(String target, boolean oldSymmetric) {
		this.target = target;
		this.oldSymmetric = oldSymmetric;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof SymmetricHistoryItem))
			return false;
		SymmetricHistoryItem item = (SymmetricHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& oldSymmetric == item.getOldSymmetric();
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(oldSymmetric);
	}

	public void setOldSymmetric(boolean oldSymmetric) {
		this.oldSymmetric = oldSymmetric;
	}

	public boolean getOldSymmetric() {
		return oldSymmetric;
	}

	@Override
	public String getShortName() {
		return "changed is symmetric";
	}

	@Override
	public String toString() {
		return "changed \"is symmetric\" status of " + target + " to "
				+ (!oldSymmetric);
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
