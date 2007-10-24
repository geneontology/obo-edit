package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

public class CardinalityHistoryItem extends LinkHistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -796392492660986705L;
	protected Integer oldValue;
	protected Integer newValue;

	public CardinalityHistoryItem(OBORestriction tr, Integer newValue) {
		this(new StringRelationship(tr), tr.getCardinality(), newValue);
	}

	public CardinalityHistoryItem() {
		this(null, null, null);
	}

	public CardinalityHistoryItem(StringRelationship rel, Integer oldValue,
			Integer newValue) {
		this.rel = rel;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CardinalityHistoryItem))
			return false;
		return super.equals(o)
				&& ObjectUtil.equals(oldValue, ((CardinalityHistoryItem) o).
						getOldValue())
				&& ObjectUtil.equals(newValue, ((CardinalityHistoryItem) o).
						getNewValue());
	}

	@Override
	public int hashCode() {
		return super.hashCode() ^ getHash(oldValue) ^ getHash(newValue);
	}

	public void setNewValue(Integer newValue) {
		this.newValue = newValue;
	}

	public void setOldValue(Integer oldValue) {
		this.oldValue = oldValue;
	}

	public Integer getNewValue() {
		return newValue;
	}

	public Integer getOldValue() {
		return oldValue;
	}

	@Override
	public String getShortName() {
		return "changed cardinality";
	}

	@Override
	public String toString() {
		return "changed cardinality of " + rel + " from " + oldValue + " to "
				+ newValue;
	}
}
