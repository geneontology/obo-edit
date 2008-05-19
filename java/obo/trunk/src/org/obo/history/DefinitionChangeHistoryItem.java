package org.obo.history;

import java.util.Collection;

import org.bbop.util.*;
import org.obo.datamodel.*;


import org.apache.log4j.*;

public class DefinitionChangeHistoryItem extends HistoryItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefinitionChangeHistoryItem.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 2073554942274780836L;
	protected String oldText;
	protected String newText;
	protected String target;

	public DefinitionChangeHistoryItem() {
		this(null, null, null);
	}

	public DefinitionChangeHistoryItem(DefinedObject defob, String def) {
		this(defob.getDefinition(), def, defob.getID());
	}

	public DefinitionChangeHistoryItem(String oldText, String newText,
			String target) {
		this.target = target;
		this.oldText = oldText;
		this.newText = newText;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof DefinitionChangeHistoryItem))
			return false;
		DefinitionChangeHistoryItem edit = (DefinitionChangeHistoryItem) o;
		boolean out = ObjectUtil.equals(target, edit.getTarget())
				&& ObjectUtil.equals(oldText, edit.getText())
				&& ObjectUtil.equals(newText, edit.getNewText());
		return out;
	}

	@Override
	public int hashCode() {
		int code = getHash(target) ^ getHash(oldText) ^ getHash(newText);
		return code;
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

	public boolean isAdd() {
		return oldText == null || oldText.length() == 0;
	}

	public boolean isDel() {
		return newText == null || newText.length() == 0;
	}

	public boolean isEdit() {
		return !isAdd() && !isDel();
	}

	@Override
	public String getShortName() {
		return "Changed definition";
	}

	@Override
	public String toString() {
		if (oldText == null || oldText.length() < 1)
			return "Set definition of " + target + " to \"" + newText + "\"";
		else if (newText == null || newText.length() < 1)
			return "Removed definition of " + target;
		else
			return "Changed definition of " + target + " from \"" + oldText
					+ "\" to \"" + newText + "\"";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
