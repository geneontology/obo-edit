package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;


import java.util.Collection;
import java.util.List;
import java.util.Collections;

import org.apache.log4j.*;

public class RangeHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RangeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 339387027507373053L;
	protected String range;
	protected String oldRange;

	public RangeHistoryItem() {
		this(null, null, null);
	}

	public RangeHistoryItem(OBOProperty target, Type range) {
		this(target.getID(), (target.getRange() == null ? null : target
				.getRange().getID()), (range == null ? null : range.getID()));
	}

	public RangeHistoryItem(String target, String oldRange, String range) {
		this.oldRange = oldRange;
		this.target = target;
		this.range = range;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof RangeHistoryItem))
			return false;
		RangeHistoryItem item = (RangeHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(range, item.getRange())
				&& ObjectUtil.equals(oldRange, item.getOldRange());
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(range) ^ getHash(oldRange);
	}

	public void setRange(String range) {
		this.range = range;
	}

	public void setOldRange(String oldRange) {
		this.oldRange = oldRange;
	}

	public String getRange() {
		return range;
	}

	public String getOldRange() {
		return range;
	}

	@Override
	public String toString() {
		return "set range of " + target + " to " + range;
	}

	@Override
	public String getShortName() {
		return "change range";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
