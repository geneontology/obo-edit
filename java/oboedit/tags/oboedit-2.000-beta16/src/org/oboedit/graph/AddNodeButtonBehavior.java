package org.oboedit.graph;

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Toolkit;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;

import org.obo.datamodel.LinkedObject;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.actions.AddAction;
import org.oboedit.gui.actions.TypedAddAction;

import edu.umd.cs.piccolo.event.PInputEvent;

public class AddNodeButtonBehavior extends AbstractClickActionButtonBehavior {

	protected AddAction addAction = new AddAction();

	protected TypedAddAction addRootAction = new TypedAddAction(false);

	protected TypedAddAction addTypeRootAction = new TypedAddAction(true);

	protected TextEditButtonBehavior textEditBehavior;

	protected JCheckBox typeCheckbox = new JCheckBox(
			"Create relationship type roots");

	protected Cursor addCursor;

	public AddNodeButtonBehavior() {
		addCursor = Toolkit.getDefaultToolkit().createCustomCursor(
				Preferences.loadLibraryImage("create_cursor.gif"),
				new Point(16, 20), "pencil");
	}

	@Override
	protected void handleClick(PInputEvent event) {
		Selection tempSelection = canvas.getPickerSelection(event);
		ClickMenuAction action = getAction(tempSelection);
		action.clickInit(tempSelection, null);
		String newID = null;
		if (action.isLegal()) {
			HistoryItem item = action.execute();
			if (item != null) {
				newID = ((TermMacroHistoryItem) item).getResult();
				SessionManager.getManager().apply(item);
			}
		}
		if (newID != null) {
			OENode node = (OENode) canvas.getNode((LinkedObject) SessionManager
					.getManager().getSession().getObject(newID));
			if (node != null) {
				textEditBehavior.setCanvas(canvas);
				textEditBehavior.showEditor(node, canvas);
			}
		}
	}

	public void setTextEditButtonBehavior(
			TextEditButtonBehavior textEditBehavior) {
		this.textEditBehavior = textEditBehavior;
	}

	@Override
	protected Cursor getCursor(PInputEvent e) {
		return addCursor;
	}

	public Icon getButtonIcon() {
		return Preferences.loadLibraryIcon("draw_node.gif");
	}

	@Override
	protected ClickMenuAction getAction(Selection tempSelection) {
		if (tempSelection == null || tempSelection.getTerms().size() == 0) {
			if (typeCheckbox.isSelected())
				return addTypeRootAction;
			else
				return addRootAction;
		} else
			return addAction;
	}

	@Override
	public JComponent getConfigurationPanel() {
		return typeCheckbox;
	}

	public String getTooltip() {
		return "<html><b>Node creation tool</b><br><hr>\n"
				+ "<table width=300><tr><td>Click an existing term to add a new is_a child. Click on empty space"
				+ "to create a new root term." + "</td></table></html>";
	}
}
