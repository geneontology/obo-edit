package org.oboedit.gui;

import java.util.List;

import org.bbop.expression.JexlContext;
import org.bbop.framework.GUIComponent;
import org.obo.datamodel.*;
import org.obo.history.HistoryItem;
import org.oboedit.gui.event.TermLoadListener;

public interface OBOTextEditComponent extends GUIComponent {

	/**
	 * Sets the currently edited object
	 */
	public void setObject(IdentifiedObject io);
	
	public IdentifiedObject getObject();
	
	public void addLoadListener(TermLoadListener listener);

	public void removeLoadListener(TermLoadListener listener);

	/**
	 * Populates any fields of the passed-in object that this component is
	 * responsible for editing.
	 */
	public void populateFields(IdentifiedObject io);

	/**
	 * Returns a list of history items. This list may be empty.
	 */
	public List<HistoryItem> getChanges();
	
	public boolean hasChanges();
	
//	public void setTextEditManager(TextEditManager textEditManager);

	public void revert();

	public void setContext(JexlContext context);

	public void setRoot(RootTextEditComponent root);
	
	public RootTextEditComponent getRoot();
}
