package org.obo.history;

import java.util.Collection;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

import org.apache.log4j.*;

public class RemoveConsiderHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RemoveConsiderHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 2470842723439947242L;

	protected String consider;

	public RemoveConsiderHistoryItem() {
	}

	public RemoveConsiderHistoryItem(ObsoletableObject target,
			ObsoletableObject consider) {
		this(target.getID(), consider.getID());
	}

	public RemoveConsiderHistoryItem(String target, String consider) {
		this.target = target;
		this.consider = consider;
	}

	public boolean equals(Object o) {
		if (!(o instanceof RemoveConsiderHistoryItem))
			return false;
		RemoveConsiderHistoryItem item = (RemoveConsiderHistoryItem) o;
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
		return "remove obsolete consider term";
	}

	@Override
	public String toString() {
		return "Removing consider replacement " + consider + " to " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
