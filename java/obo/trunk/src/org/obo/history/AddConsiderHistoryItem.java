package org.obo.history;

import java.util.Collection;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class AddConsiderHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddConsiderHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 555537752820316709L;

	protected String consider;

	public AddConsiderHistoryItem() {
	}

	public AddConsiderHistoryItem(ObsoletableObject target,
			ObsoletableObject consider) {
		this(target.getID(), consider.getID());
	}

	public AddConsiderHistoryItem(String target, String consider) {
		this.target = target;
		this.consider = consider;
	}

	public boolean equals(Object o) {
		if (!(o instanceof AddConsiderHistoryItem))
			return false;
		AddConsiderHistoryItem item = (AddConsiderHistoryItem) o;
		return ObjectUtil.equals(item.getConsider(), consider)
				&& ObjectUtil.equals(item.getTarget(), target);
	}

	public String getConsider() {
		return consider;
	}

	public void setConsider(String consider) {
		this.consider = consider;
	}

	@Override
	public String getShortName() {
		return "add obsolete consider term";
	}

	@Override
	public String toString() {
		return "Adding consider replacement " + consider + " to " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		// this really shouldn't even be able to happen
		// id forwards should not be possible with obsolete terms
		return null;
	}

}
