package org.oboedit.graph;

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;

import org.obo.datamodel.RootAlgorithm;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.actions.DeleteAction;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;

public class EraseButtonBehavior extends AbstractClickActionButtonBehavior {

	protected DeleteAction delAction = new DeleteAction(false);

	protected JCheckBox destroyCheckbox = new JCheckBox(
			"Permanently destroy deleted terms");

	protected LinkDatabaseCanvas canvas;

	protected Cursor eraseCursor;

	public EraseButtonBehavior() {
		destroyCheckbox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				delAction.setShouldDestroy(destroyCheckbox.isSelected());
			}
		});
		eraseCursor = Toolkit.getDefaultToolkit().createCustomCursor(
				Preferences.loadLibraryImage("erase_cursor.gif"),
				new Point(4, 28), "eraser");
	}

	@Override
	protected Cursor getActiveCursor() {
		return eraseCursor;
	}

	public Icon getButtonIcon() {
		return Preferences.loadLibraryIcon("erase.gif");
	}

	public JComponent getConfigurationPanel() {
		return destroyCheckbox;
	}

	@Override
	protected ClickMenuAction getAction(Selection tempSelection) {
		return delAction;
	}

	public String getTooltip() {
		return "<html><b>Eraser tool tool</b><br><hr>\n"
				+ "<table width=300><tr><td>Click an existing term or link " +
						"(or group of selected terms and links) to delete them."
				+ "</td></table></html>";
	}
}
