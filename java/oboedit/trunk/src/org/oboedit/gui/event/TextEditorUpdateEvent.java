package org.oboedit.gui.event;

import java.util.*;

import org.obo.datamodel.*;

/**
 * An update to the text editor. This kind of event is intended for use with
 * plugins that fetch some additional information about a term and give the
 * option to fill in the text editor with that info (the change is not committed
 * until the commit button is clicked).
 * 
 * This event will eventually allow every aspect of a the text editor to be
 * updated. For now, only the definition can be updated. This class has been
 * constructed in such a way that older code will not be broken as this class
 * grows.
 * 
 * Fields that are set to null are ignored, NOT reset. To reset a value, set it
 * to an empty value, such as "" or an empty vector.
 */

public class TextEditorUpdateEvent extends EventObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected String newDefinition = null;
	protected Vector dbxrefChanges;

	public TextEditorUpdateEvent(Object source) {
		super(source);
	}

	public static class DbxrefUpdate {
		protected boolean isAdd;
		protected boolean isDelete;
		protected Dbxref oldDbxref;
		protected Dbxref newDbxref;

		public DbxrefUpdate(Dbxref newDbxref, Dbxref oldDbxref, boolean isAdd,
				boolean isDelete) {
			this.isAdd = isAdd;
			this.isDelete = isDelete;
			this.newDbxref = newDbxref;
			this.oldDbxref = oldDbxref;
		}

		public Dbxref getOldDbxref() {
			return oldDbxref;
		}

		public Dbxref getNewDbxref() {
			return newDbxref;
		}

		public boolean isAdd() {
			return isAdd;
		}

		public boolean isDelete() {
			return isDelete;
		}
	}

	public void setDbxrefUpdates(Vector in) {
		dbxrefChanges = in;
	}

	public Vector getDbxrefUpdates() {
		return dbxrefChanges;
	}

	public void setNewDefinition(String newDefinition) {
		this.newDefinition = newDefinition;
	}

	public String getNewDefinition() {
		return newDefinition;
	}
}
