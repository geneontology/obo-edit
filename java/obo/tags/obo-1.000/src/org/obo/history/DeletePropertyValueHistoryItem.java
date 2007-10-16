package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;


public class DeletePropertyValueHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5400055973408890140L;
	protected String propertyID;
	protected String datatypeID;
	protected String value;

	public DeletePropertyValueHistoryItem(ValueLink ipv) {
		setTarget(ipv.getChild().getID());
		propertyID = ipv.getType().getID();
		if (ipv.getValue() instanceof DatatypeValue) {
			DatatypeValue dv = (DatatypeValue) ipv.getValue();
			value = dv.getValue();
			datatypeID = dv.getType().getID();
		} else {
			value = ((IdentifiedObject) ipv.getValue()).getID();
		}
	}

	public DeletePropertyValueHistoryItem(String targetID, String propertyID,
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
		if (!(o instanceof DeletePropertyValueHistoryItem))
			return false;
		DeletePropertyValueHistoryItem item = (DeletePropertyValueHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(propertyID, item.getPropertyID())
				&& ObjectUtil.equals(datatypeID, item.getDatatypeID())
				&& ObjectUtil.equals(value, item.getValue());
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
