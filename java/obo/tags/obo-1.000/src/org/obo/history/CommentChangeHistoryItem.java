package org.obo.history;

import java.util.Collection;

import org.bbop.util.*;
import org.obo.datamodel.*;


public class CommentChangeHistoryItem extends HistoryItem {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1042267529017580807L;
	protected String oldText;
	protected String newText;
	protected String target;

	public CommentChangeHistoryItem() {
		this(null, null, null);
	}

	public CommentChangeHistoryItem(CommentedObject comob, String comment) {
		this(comob.getComment(), comment, comob.getID());
	}

	public CommentChangeHistoryItem(String oldText, String newText,
			String target) {
		this.oldText = oldText;
		this.newText = newText;
		this.target = target;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CommentChangeHistoryItem))
			return false;
		CommentChangeHistoryItem edit = (CommentChangeHistoryItem) o;
		return ObjectUtil.equals(target, edit.getTarget())
				&& ObjectUtil.equals(oldText, edit.getText())
				&& ObjectUtil.equals(newText, edit.getNewText());
	}

	@Override
	public int hashCode() {
		return getHash(target) ^ getHash(oldText) ^ getHash(newText);
	}

	public void setNewText(String newText) {
		this.newText = newText;
	}

	public void setOldText(String oldText) {
		this.oldText = oldText;
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

	@Override
	public String getShortName() {
		return "COMMENT_CHANGE";
	}

	@Override
	public String toString() {
		if (oldText == null || oldText.length() < 1)
			return "Added comment \"" + newText + "\"";
		else if (newText == null || newText.length() < 1)
			return "Removed comment";
		else
			return "Changed comment from \"" + oldText + "\" to \"" + newText
					+ "\"";
	}

	@Override
	public HistoryList forwardID(String oldID, Collection newIDs) {
		return null;
	}
}
