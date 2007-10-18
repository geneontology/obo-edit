package org.obo.history;

import java.util.Collection;

import org.bbop.util.*;
import org.obo.datamodel.*;


public class NamespaceHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5475209155866979138L;
	protected Namespace oldns;
	protected Namespace newns;
	protected String target;

	public NamespaceHistoryItem(IdentifiedObject io, Namespace newns) {
		this(io.getNamespace(), newns, io.getID());
	}

	public NamespaceHistoryItem(Namespace oldns, Namespace newns, String target) {
		this.target = target;
		this.oldns = oldns;
		this.newns = newns;
	}

	public NamespaceHistoryItem() {
		this(null, null, null);
	}

	public void complete() {
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof NamespaceHistoryItem))
			return false;
		NamespaceHistoryItem edit = (NamespaceHistoryItem) o;
		return ObjectUtil.equals(target, edit.getTarget())
				&& ObjectUtil.equals(oldns, edit.getOldNamespace())
				&& ObjectUtil.equals(newns, edit.getNewNamespace());
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(oldns) ^ getHash(newns);
	}

	public void setOldNamespace(Namespace oldns) {
		this.oldns = oldns;
	}

	public void setNewNamespace(Namespace newns) {
		this.newns = newns;
	}

	@Override
	public String getTarget() {
		return target;
	}

	@Override
	public void setTarget(String target) {
		this.target = target;
	}

	public Namespace getNewNamespace() {
		return newns;
	}

	public Namespace getOldNamespace() {
		return oldns;
	}

	@Override
	public String getShortName() {
		return "Namespace change";
	}

	@Override
	public String toString() {
		if (oldns == null)
			return "Assigned namespace \"" + newns + "\" to " + target;
		else if (newns == null)
			return "Removed namespace \"" + oldns + "\" from " + target;
		else
			return "Changed namespace of " + target + " from \"" + oldns
					+ "\" to \"" + newns + "\"";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
