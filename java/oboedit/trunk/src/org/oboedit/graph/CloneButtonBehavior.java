package org.oboedit.graph;

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Toolkit;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.apache.log4j.Logger;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.actions.CloneAction;
import org.oboedit.gui.components.LinkDatabaseCanvas;

public class CloneButtonBehavior extends AbstractClickActionButtonBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CloneButtonBehavior.class);

	protected CloneAction cloneAction = new CloneAction();

	protected LinkDatabaseCanvas canvas;

	protected Cursor cloneCursor;

	public CloneButtonBehavior() {
		cloneCursor = Toolkit.getDefaultToolkit().createCustomCursor(
				Preferences.loadLibraryImage("clone_cursor.gif"),
				new Point(16, 20), "clone_tool");
	}

	@Override
	protected Cursor getActiveCursor() {
		return cloneCursor;
	}

	public Icon getButtonIcon() {
		return Preferences.loadLibraryIcon("clone.gif");
	}

	public JComponent getConfigurationPanel() {
		return null;
	}

	@Override
	protected ClickMenuAction getAction(Selection tempSelection) {
		return cloneAction;
	}

	public String getTooltip() {
		return "<html><b>Clone tool</b><br><hr>\n"
				+ "<table width=300><tr><td>Click an existing term (or group " +
						"of selected terms) to create clones of"
				+ " the target term(s)." + "</td></table></html>";
	}
}
