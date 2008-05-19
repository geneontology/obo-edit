package org.obo.history;

import java.util.Collection;

import org.bbop.util.*;
import org.obo.datamodel.*;


import org.apache.log4j.*;

public class NameChangeHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameChangeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 6784769526900877984L;

	protected String oldText;

	protected String newText;

	protected String target;

	public NameChangeHistoryItem() {
		this(null, null, null);
	}

	public NameChangeHistoryItem(String newText, String oldText, String target) {
		this.target = target;
		this.oldText = oldText;
		this.newText = newText;
	}

	public NameChangeHistoryItem(IdentifiedObject io, String newText) {
		this(newText, io.getName(), io.getID());
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof NameChangeHistoryItem) {
			NameChangeHistoryItem edit = (NameChangeHistoryItem) o;
			return ObjectUtil.equals(target, edit.getTarget())
					&& ObjectUtil.equals(oldText, edit.getText())
					&& ObjectUtil.equals(newText, edit.getNewText());
		} else
			return false;
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(oldText) ^ getHash(newText);
	}

	public void setOldText(String oldText) {
		this.oldText = oldText;
	}

	public void setNewText(String newText) {
		this.newText = newText;
	}

	@Override
	public String getTarget() {
		return target;
	}

	@Override
	public void setTarget(String target) {
		this.target = target;
	}

	public String getText() {
		return oldText;
	}

	public String getNewText() {
		return newText;
	}

	public String getOldText() {
		return oldText;
	}

	@Override
	public String getShortName() {
		return "Name change";
	}

	@Override
	public String toString() {
		return "Changed name of " + target + " from \"" + oldText + "\" to \""
				+ newText + "\"";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return defaultForwardID(this, oldID, newIDs);
	}
}
