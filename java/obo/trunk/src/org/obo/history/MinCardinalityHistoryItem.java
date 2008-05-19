package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class MinCardinalityHistoryItem extends LinkHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MinCardinalityHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -3968536739000558894L;
	protected Integer oldValue;
	protected Integer newValue;

	public MinCardinalityHistoryItem(OBORestriction tr, Integer newValue) {
		this(new StringRelationship(tr), tr.getMinCardinality(), newValue);
	}

	public MinCardinalityHistoryItem() {
		this(null, null, null);
	}

	public MinCardinalityHistoryItem(StringRelationship rel, Integer oldValue,
			Integer newValue) {
		this.rel = rel;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}

	@Override
	public int hashCode() {
		return getHash(rel) ^ getHash(oldValue) ^ getHash(newValue);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof MinCardinalityHistoryItem))
			return false;
		MinCardinalityHistoryItem item = (MinCardinalityHistoryItem) o;
		return ObjectUtil.equals(item.getRel(), rel)
				&& ObjectUtil.equals(oldValue, item.getOldValue())
				&& ObjectUtil.equals(newValue, item.getNewValue());
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
		return "changed min cardinality";
	}

	@Override
	public String toString() {
		return "changed min cardinality of " + rel + " from " + oldValue
				+ " to " + newValue;
	}
}
