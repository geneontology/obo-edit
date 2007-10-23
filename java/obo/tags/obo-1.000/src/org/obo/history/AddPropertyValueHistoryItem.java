package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

public class AddPropertyValueHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7801211877188143037L;
	protected String propertyID;
	protected String datatypeID;
	protected String value;

	public AddPropertyValueHistoryItem(String targetID, String propertyID,
			String datatypeID, String value) {
		setTarget(targetID);
		this.propertyID = propertyID;
		this.datatypeID = datatypeID;
		this.value = value;
	}

	public String getPropertyID() {
		return propertyID;
	}

	public String getDatatypeID() {
		return datatypeID;
	}

	public String getValue() {
		return value;
	}

	@Override
	public String getShortName() {
		return "add property value";
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof AddPropertyValueHistoryItem))
			return false;
		AddPropertyValueHistoryItem item = (AddPropertyValueHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(propertyID, item.getPropertyID())
				&& ObjectUtil.equals(datatypeID, item.getDatatypeID())
				&& ObjectUtil.equals(value, item.getValue());
	}
	
	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		// Ignore this for now; it's not clear whether merges of instances
		// are even legal
		return null;
	}
}
