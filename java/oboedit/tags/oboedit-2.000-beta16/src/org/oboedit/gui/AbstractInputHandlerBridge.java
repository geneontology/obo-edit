package org.oboedit.gui;

import java.awt.Point;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.List;

import javax.swing.JComponent;

import org.bbop.util.ObjectUtil;
import org.oboedit.controller.EditActionManager;

public abstract class AbstractInputHandlerBridge implements DropTargetListener,
		MouseListener, KeyListener, MouseMotionListener {
	boolean dragging = false;

	public AbstractInputHandlerBridge() {
	}

	protected JComponent component;

	protected InputHandlerI currentDragHandler;

	protected boolean dragged = false;

	public void setComponent(JComponent component) {
		this.component = component;
	}

	public void drop(DropTargetDropEvent e) {
		try {
			GestureTarget target = getTarget(e.getLocation().getX(), e
					.getLocation().getY());

			Object transferData = DropUtil.getSelection(e);

			boolean b = currentDragHandler.drop(component, transferData,
					target, e.getLocation(), EditActionManager.getManager()
							.getKeyRecorder().getKeyChecker());
			e.dropComplete(b);
		} catch (InvalidDnDOperationException ex) {
			// ignore it
		}
		dragging = false;
	}

	public abstract GestureTarget getTarget(double x, double y);

	public void dragOver(DropTargetDragEvent e) {
		if (!allowDrop(e)) {
			e.rejectDrag();
			return;
		} else
			e.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE);
	}

	public boolean allowDrop(DropTargetDragEvent e) {
		Object data = DropUtil.getSelection(e);
		if (data != null) {
			GestureTarget target = getTarget(e.getLocation().getX(), e
					.getLocation().getY());
			int allowDropVal = setCurrentHandler(data, target, e);
			if (allowDropVal != InputHandlerI.REJECT_DROP) {
				almostDropDelegate(getCurrentHandler());
				return true;
			}
			// java.util.List<InputHandlerI> l = EditActionManager.getManager()
			// .getInputHandlers();
			// for (int i = 0; i < l.size(); i++) {
			// InputHandlerI handler = (InputHandlerI) l.get(i);
			// int allowDropVal = handler.allowDrop(component, data, target, e
			// .getLocation(), EditActionManager.getManager()
			// .getKeyRecorder().getKeyChecker());
			// if (allowDropVal == InputHandlerI.ALMOST_ACCEPT_DROP
			// || allowDropVal == InputHandlerI.ACCEPT_DROP) {
			// almostDropDelegate(handler);
			// currentDragHandler = handler;
			// return allowDropVal == InputHandlerI.ACCEPT_DROP;
			// }
			// }
			return false;

		}
		return false;
	}

	public InputHandlerI getCurrentHandler() {
		return currentDragHandler;
	}

	public void setCurrentHandler(InputHandlerI handler) {
		this.currentDragHandler = handler;
	}

	protected List<InputHandlerI> getAllDragInputHandlers() {
		return EditActionManager.getManager().getInputHandlers();
	}
	
	protected List<InputHandlerI> getAllClickInputHandlers() {
		return EditActionManager.getManager().getInputHandlers();
	}

	protected InputHandlerI getDefaultInputHandler() {
		return EditActionManager.getManager().getDefaultInputHandler();
	}

	protected int setCurrentHandler(Object data, GestureTarget target,
			DropTargetDragEvent e) {
		setCurrentHandler(null);
		java.util.List<InputHandlerI> l = getAllDragInputHandlers();
		for (int i = 0; i < l.size(); i++) {
			InputHandlerI handler = (InputHandlerI) l.get(i);
			if (ObjectUtil.equals(handler, getDefaultInputHandler()))
				continue;
			int allowDropVal = handler.allowDrop(component, data, target, e
					.getLocation(), EditActionManager.getManager()
					.getKeyRecorder().getKeyChecker());
			if (allowDropVal == InputHandlerI.ALMOST_ACCEPT_DROP
					|| allowDropVal == InputHandlerI.ACCEPT_DROP) {
				setCurrentHandler(handler);
				return allowDropVal;
			}
		}
		if (getDefaultInputHandler() != null) {
			InputHandlerI handler = getDefaultInputHandler();
			int allowDropVal = handler.allowDrop(
					component,
					data,
					target,
					e.getLocation(),
					EditActionManager.getManager().getKeyRecorder()
							.getKeyChecker());
			setCurrentHandler(getDefaultInputHandler());
			return allowDropVal;
		}
		return InputHandlerI.REJECT_DROP;
	}

	protected void almostDropDelegate(InputHandlerI handler) {
	}

	public void dragEnter(DropTargetDragEvent e) {
		if (!allowDrop(e)) {
			e.rejectDrag();
			return;
		} else
			e.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE);
	}

	public void dragExit(DropTargetEvent e) {
	}

	public void dropActionChanged(DropTargetDragEvent dtde) {
	}

	public JComponent getComponent() {
		return component;
	}

	public void mousePressed(MouseEvent e) {
		dragged = false;
	}

	protected void processClick(MouseEvent e) {
		if (dragged)
			return;
		GestureTarget target = getTarget(e.getX(), e.getY());
		java.util.List<InputHandlerI> l = getAllClickInputHandlers();
		for (int i = 0; i < l.size(); i++) {
			InputHandlerI handler = (InputHandlerI) l.get(i);
			if (ObjectUtil.equals(handler, getDefaultInputHandler()))
				continue;
			if (handler.click(component, target, e, EditActionManager
					.getManager().getKeyRecorder().getKeyChecker()))
				return;
		}
		if (getDefaultInputHandler() != null)
			getDefaultInputHandler().click(
					component,
					target,
					e,
					EditActionManager.getManager().getKeyRecorder()
							.getKeyChecker());
	}

	public void mouseClicked(MouseEvent e) {
		dragged = false;
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	public void mouseReleased(MouseEvent e) {
		processClick(e);
		dragged = false;
	}

	public void keyPressed(KeyEvent e) {
		processPress(e);
	}

	public void keyReleased(KeyEvent e) {
		processPress(e);
	}

	public void keyTyped(KeyEvent e) {
		processPress(e);
	}

	protected void processPress(KeyEvent e) {
		java.util.List<InputHandlerI> l = getAllClickInputHandlers();
		for (int i = 0; i < l.size(); i++) {
			InputHandlerI handler = (InputHandlerI) l.get(i);
			if (ObjectUtil.equals(handler, getDefaultInputHandler()))
				continue;
			if (handler.press(component, e, EditActionManager.getManager()
					.getKeyRecorder().getKeyChecker()))
				return;
		}
		if (getDefaultInputHandler() != null)
			getDefaultInputHandler().press(
					component,
					e,
					EditActionManager.getManager().getKeyRecorder()
							.getKeyChecker());
	}

	public void mouseDragged(MouseEvent e) {
		dragged = true;
	}

	public void mouseMoved(MouseEvent e) {
	}
}
