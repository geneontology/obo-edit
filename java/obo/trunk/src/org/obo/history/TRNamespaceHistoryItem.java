package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class TRNamespaceHistoryItem extends LinkHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TRNamespaceHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -5089048862557050015L;
	protected String oldns;
	protected String newns;

	public TRNamespaceHistoryItem() {
		this(null, null, null);
	}

	public TRNamespaceHistoryItem(StringRelationship rel, String newns,
			String oldns) {
		this.rel = rel;
		this.oldns = oldns;
		this.newns = newns;
	}

	public TRNamespaceHistoryItem(Link tr, Namespace newns) {
		this(createStringRelationship(tr), (newns == null ? null : newns
				.getID()), (tr.getNamespace() == null ? null : tr
				.getNamespace().getID()));
	}

	@Override
	public int hashCode() {
		return getHash(rel) ^ getHash(oldns) ^ getHash(newns);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TRNamespaceHistoryItem))
			return false;
		TRNamespaceHistoryItem item = (TRNamespaceHistoryItem) o;
		return ObjectUtil.equals(rel, item.getRel())
				&& ObjectUtil.equals(oldns, item.getOldNamespace())
				&& ObjectUtil.equals(newns, item.getNewNamespace());
	}

	public void setOldNamespace(String oldns) {
		this.oldns = oldns;
	}

	public void setNewNamespace(String newns) {
		this.newns = newns;
	}

	public String getOldNamespace() {
		return oldns;
	}

	public String getNewNamespace() {
		return newns;
	}

	@Override
	public String getShortName() {
		return "Term Relationship Namespace Change";
	}

	@Override
	public String toString() {
		return "Changed namespace of " + rel + " from " + oldns + " to "
				+ newns;
	}
}
