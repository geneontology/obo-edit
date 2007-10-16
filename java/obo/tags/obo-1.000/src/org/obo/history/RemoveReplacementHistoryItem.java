package org.obo.history;

import java.util.Collection;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;

public class RemoveReplacementHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5821629437500675686L;

	protected String replace;

	public RemoveReplacementHistoryItem() {
	}

	public RemoveReplacementHistoryItem(ObsoletableObject target,
			ObsoletableObject replace) {
		this(target.getID(), replace.getID());
	}

	public RemoveReplacementHistoryItem(String target, String replace) {
		this.target = target;
		this.replace = replace;
	}

	public boolean equals(Object o) {
		if (!(o instanceof RemoveReplacementHistoryItem))
			return false;
		RemoveReplacementHistoryItem item = (RemoveReplacementHistoryItem) o;
		return ObjectUtil.equals(item.getTarget(), target)
				&& ObjectUtil.equals(item.getReplace(), replace);
	}

	public String getReplace() {
		return replace;
	}

	public void setReplace(String replace) {
		this.replace = replace;
	}

	@Override
	public String getShortName() {
		return "remove obsolete replacement term";
	}

	@Override
	public String toString() {
		return "Removing replacement " + replace + " to " + target;
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
