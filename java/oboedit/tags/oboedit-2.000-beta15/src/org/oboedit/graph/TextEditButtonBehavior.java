package org.oboedit.graph;

import java.awt.AWTException;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

import org.bbop.swing.XMLLayoutUtil;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.components.TextEditor;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;

public class TextEditButtonBehavior implements ToolbarButtonBehavior {

	protected PBasicInputEventHandler handler = new PBasicInputEventHandler() {
		@Override
		public void mousePressed(PInputEvent event) {
			OENode node = (OENode) PiccoloUtil.getNodeOfClass(event.getPath(),
					OENode.class);
			if (node != null) {
				showEditor(node, canvas);
			}
		}

		@Override
		public void mouseEntered(PInputEvent event) {
			OENode node = (OENode) PiccoloUtil.getNodeOfClass(event.getPath(),
					OENode.class);
			if (node != null) {
				canvas
						.setCursor(Cursor
								.getPredefinedCursor(Cursor.TEXT_CURSOR));
			}
		}

		@Override
		public void mouseExited(PInputEvent event) {
			canvas.setCursor(Cursor.getDefaultCursor());
		}

		@Override
		public void keyPressed(PInputEvent event) {
			if (event.getKeyCode() == KeyEvent.VK_ESCAPE) {
				hideEditor();
			}
		}
	};

	protected MouseListener editorFocusListener = new MouseAdapter() {
		@Override
		public void mouseExited(MouseEvent e) {
			commitEdits();
		}
	};

	protected TextEditor editor;

	protected LinkDatabaseCanvas canvas;

	protected Robot robot;
	
	protected Action commitAction = new AbstractAction() {
		public void actionPerformed(ActionEvent e) {
			commitEdits();			
		}
	};

	public TextEditButtonBehavior() {
		try {
			robot = new Robot();
			robot.setAutoDelay(0);
			robot.setAutoWaitForIdle(false);
		} catch (AWTException e) {
			e.printStackTrace();
		}
	}

	public void showEditor(OENode node, LinkDatabaseCanvas canvas) {
		if (editor == null) {
			// for reasons I don't understand, this doesn't work without this
			// initial call
			// to loadObject
			// textEditManager.forceLoad((LinkedObject) node.getObject());
			editor = new TextEditor("TEXTEDITOR");
			editor.init();
			editor.reload();
		}

		canvas.getPlacementPanel().removeAll();
		canvas.getPlacementPanel().setLayout(null);
		canvas.getPlacementPanel().add(editor);
		canvas.setPlacementPanelVisible(true);
		editor.setLocation(100, 100);
		editor.setSize(300, 400);
		editor.setObject((IdentifiedObject) node.getObject());

		XMLLayoutUtil.guiupdateTree(editor);

		if (robot != null) {
			Point p = new Point(editor.getLocation());
			SwingUtilities.convertPointToScreen(p, editor.getParent());
			robot.mouseMove(p.x + editor.getWidth() / 2, p.y
					+ editor.getHeight() / 2);
		}
		editor.addMouseListener(editorFocusListener);
		editor.requestFocus();
	}

	protected void commitEdits() {
		editor.commit();
		hideEditor();
	}

	protected void hideEditor() {
		canvas.getPlacementPanel().removeAll();
		canvas.setPlacementPanelVisible(false);
		canvas.repaint();
		editor.removeMouseListener(editorFocusListener);
	}
	
	public void setCanvas(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;		
	}

	public void activate(LinkDatabaseCanvas canvas) {
		setCanvas(canvas);
		canvas.addInputEventListener(handler);
	}

	public void deactivate(LinkDatabaseCanvas canvas) {
		hideEditor();
		canvas.removeInputEventListener(handler);
		setCanvas(null);
	}

	public Icon getButtonIcon() {
		return Preferences.loadLibraryIcon("text_edit.gif");
	}

	public String getButtonLabel() {
		return null;
	}

	public JComponent getConfigurationPanel() {
		return null;
	}
	
	public String getTooltip() {
		return "<html><b>Text edit tool</b><br><hr>\n"
				+ "<table width=300><tr><td>Click a term to display the text editor."
				+ "</td></table></html>";
	}
}
