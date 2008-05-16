package org.oboedit.graph;

import java.awt.Point;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import javax.swing.SwingUtilities;

import org.oboedit.gui.AbstractInputHandlerBridge;
import org.oboedit.gui.AbstractSelectableHandlerBridge;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.components.GraphEditor;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.util.PPickPath;

import org.apache.log4j.*;

public class DragDropEditBehavior implements ViewBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DragDropEditBehavior.class);
	protected class InputListener extends AbstractSelectableHandlerBridge implements
			PInputEventListener {
		LinkDatabaseCanvas canvas;

		public InputListener(LinkDatabaseCanvas canvas) {
			this.canvas = canvas;
			setComponent(canvas);
		}

		@Override
		public GestureTarget getTarget(double x, double y) {
			GestureTarget target = ((LinkDatabaseCanvas) getComponent())
					.getTarget((int) x, (int) y);
			return target;
		}
		
		@Override
		public void drop(DropTargetDropEvent e) {
			super.drop(e);
			if (canvas instanceof GraphEditor) {
				GraphEditor editor = (GraphEditor) canvas;
				editor.completeDrop();
			}
		}

		public void processEvent(PInputEvent event, int type) {
			if (event.isRightMouseButton()) {
				PNode node = event.getPath().getPickedNode();
				if (PiccoloUtil.rightClickMenusDisabled(node))
					return;
			}
			switch (type) {
			case KeyEvent.KEY_PRESSED:
				keyPressed((KeyEvent) event.getSourceSwingEvent());
				break;

			case KeyEvent.KEY_RELEASED:
				keyReleased((KeyEvent) event.getSourceSwingEvent());
				break;

			case KeyEvent.KEY_TYPED:
				keyTyped((KeyEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_CLICKED:
				mouseClicked((MouseEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_DRAGGED:
				mouseDragged((MouseEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_ENTERED:
				mouseEntered((MouseEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_EXITED:
				mouseExited((MouseEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_MOVED:
				mouseMoved((MouseEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_PRESSED:
				mousePressed((MouseEvent) event.getSourceSwingEvent());
				break;

			case MouseEvent.MOUSE_RELEASED:
				mouseReleased((MouseEvent) event.getSourceSwingEvent());
				break;
			}
		}
	};

	protected InputListener inputListener;
	
	public InputListener getInputListener() {
		return inputListener;
	}

	public void install(LinkDatabaseCanvas canvas) {
		inputListener = new InputListener(canvas);

		canvas.addDropTargetListener(inputListener);
		canvas.addInputEventListener(inputListener);
		/*
		 * canvas.addMouseListener(inputListener);
		 * canvas.addMouseMotionListener(inputListener);
		 * canvas.addKeyListener(inputListener);
		 */
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeDropTargetListener(inputListener);
		canvas.removeInputEventListener(inputListener);
		/*
		canvas.removeMouseListener(inputListener);
		canvas.removeMouseMotionListener(inputListener);
		canvas.removeKeyListener(inputListener);
		*/

	}

}
