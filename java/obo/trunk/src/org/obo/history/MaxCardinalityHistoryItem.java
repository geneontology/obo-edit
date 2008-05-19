package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class MaxCardinalityHistoryItem extends LinkHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MaxCardinalityHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4845681425484012581L;
	protected Integer oldValue;
	protected Integer newValue;

	public MaxCardinalityHistoryItem() {
		this(null, null, null);
	}

	public MaxCardinalityHistoryItem(OBORestriction tr, Integer newValue) {
		this(new StringRelationship(tr), tr.getMaxCardinality(), newValue);
	}

	public MaxCardinalityHistoryItem(StringRelationship rel, Integer oldValue,
			Integer newValue) {
		this.rel = rel;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof MaxCardinalityHistoryItem))
			return false;
		MaxCardinalityHistoryItem item = (MaxCardinalityHistoryItem) o;
		return ObjectUtil.equals(rel, item.getRel())
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
		return "changed max cardinality";
	}

	@Override
	public String toString() {
		return "changed max cardinality of " + rel + " from " + oldValue
				+ " to " + newValue;
	}
}
