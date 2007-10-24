package org.oboedit.graph;

import java.awt.Cursor;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.obo.history.HistoryItem;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.Selection;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;

public abstract class AbstractClickActionButtonBehavior implements
		ToolbarButtonBehavior {
	protected PInputEventListener mouseListener = new PBasicInputEventHandler() {
		@Override
		public void mouseClicked(PInputEvent event) {
			handleClick(event);
		}
		
		@Override
		public void mouseEntered(PInputEvent event) {
			Cursor c = getCursor(event);
			if (c == null)
				c = Cursor.getDefaultCursor();
			canvas.setCursor(c);
		}

		@Override
		public void mouseExited(PInputEvent event) {
			canvas.setCursor(Cursor.getDefaultCursor());
		}
	};
	
	protected LinkDatabaseCanvas canvas;

	protected abstract ClickMenuAction getAction(Selection tempSelection);

	protected void handleClick(PInputEvent event) {
		Selection tempSelection = canvas.getPickerSelection(event);
		ClickMenuAction action = getAction(tempSelection);
		action.clickInit(tempSelection, null);
		if (action.isLegal()) {
			HistoryItem item = action.execute();
			if (item != null)
				SessionManager.getManager().apply(item);
		}	
	}
	
	protected Cursor getCursor(PInputEvent e) {
		PCNode node = (PCNode) PiccoloUtil.getNodeOfClass(e.getPath(),
				PCNode.class);
		if (node != null && isLegalTarget(node))
			return getActiveCursor();
		else
			return Cursor.getDefaultCursor();
	}
	
	protected Cursor getActiveCursor() {
		return null;
	}
	
	protected boolean isLegalTarget(PCNode node) {
		return true;
	}

	public void activate(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addInputEventListener(mouseListener);
	}

	public void deactivate(LinkDatabaseCanvas canvas) {
		this.canvas.removeInputEventListener(mouseListener);
		this.canvas = null;
	}

	public Icon getButtonIcon() {
		return null;
	}

	public String getButtonLabel() {
		return null;
	}

	public JComponent getConfigurationPanel() {
		return null;
	}
}
