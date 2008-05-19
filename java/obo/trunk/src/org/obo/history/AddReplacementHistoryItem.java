package org.obo.history;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class AddReplacementHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddReplacementHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -1400123185174542266L;

	protected String replace;

	public AddReplacementHistoryItem() {
	}

	public AddReplacementHistoryItem(ObsoletableObject target,
			ObsoletableObject replace) {
		this(target.getID(), replace.getID());
	}

	public AddReplacementHistoryItem(String target, String replace) {
		this.target = target;
		this.replace = replace;
	}

	public boolean equals(Object o) {
		if (!(o instanceof AddReplacementHistoryItem))
			return false;
		AddReplacementHistoryItem item = (AddReplacementHistoryItem) o;
		return item.getTarget().equals(target)
				&& item.getReplace().equals(replace);
	}

	public String getReplace() {
		return replace;
	}

	public void setReplace(String replace) {
		this.replace = replace;
	}

	@Override
	public String getShortName() {
		return "add obsolete replacement term";
	}

	@Override
	public String toString() {
		return "Adding replacement " + replace + " to " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
