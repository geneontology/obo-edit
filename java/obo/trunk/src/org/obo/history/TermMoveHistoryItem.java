package org.obo.history;

import org.bbop.util.*;
import org.obo.datamodel.*;

import java.util.*;

import org.apache.log4j.*;

public class TermMoveHistoryItem extends SubclassedMacroHistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermMoveHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4038576748144412475L;
	StringRelationship rel;
	String newProp;

	public TermMoveHistoryItem(String target, String newProp,
			StringRelationship rel) {
		super("move");
		this.target = target;
		this.rel = rel;
		this.newProp = newProp;
		/*
		 * edited.add(target); edited.add(rel.getChild());
		 * edited.add(rel.getParent());
		 */
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TermMoveHistoryItem))
			return false;
		TermMoveHistoryItem item = (TermMoveHistoryItem) o;
		return ObjectUtil.equals(target, item.getTarget())
				&& ObjectUtil.equals(rel, item.getRelationship());
	}

	@Override
	public int hashCode() {
		return getHash(rel) ^ getHash(target);
	}

	public TermMoveHistoryItem() {
		this((String) null, null, null);
	}

	public void setRel(StringRelationship rel) {
		this.rel = rel;
	}

	public void setNewProp(String newProp) {
		this.newProp = newProp;
	}

	public TermMoveHistoryItem(LinkedObject target, OBOProperty newProp,
			Link rel) {
		this(target.getID(), newProp.getID(), createStringRelationship(rel));
	}

	public TermMoveHistoryItem(LinkedObject target, Link rel) {
		this(target, rel.getType(), rel);
	}

	@Override
	protected OperationWarning getItems(OBOSession history, List historyItems) {
		historyItems.add(new CreateLinkHistoryItem(rel.getChild(), newProp,
				target));
		historyItems.add(new DeleteLinkHistoryItem(rel));
		return null;
	}

	@Override
	public String getShortName() {
		return "move";
	}

	public StringRelationship getRelationship() {
		return rel;
	}

	@Override
	public String toString() {
		if (rel == null)
			return "moved";
		return "Moved " + rel.getChild() + " to " + target + " from "
				+ rel.getParent();
	}
}
