package org.oboedit.controller;

import java.util.LinkedList;
import java.util.List;

import org.bbop.swing.KeyRecorder;
import org.oboedit.gui.ClickMenuAction;
import org.oboedit.gui.DropMenuAction;
import org.oboedit.gui.InputHandlerI;

public class EditActionManager {
	protected List<DropMenuAction> dropMenuActions = new LinkedList<DropMenuAction>();

	protected List<ClickMenuAction> editActions = new LinkedList<ClickMenuAction>();

	protected List<InputHandlerI> inputHandlers = new LinkedList<InputHandlerI>();

	protected static EditActionManager manager;

	protected KeyRecorder keyRecorder = new KeyRecorder();

	protected InputHandlerI defaultInputHandler;

	public KeyRecorder getKeyRecorder() {
		return keyRecorder;
	}

	public EditActionManager() {
		keyRecorder.setCoalesceInterval(600);
		keyRecorder.install();
	}

	public static EditActionManager getManager() {
		if (manager == null)
			manager = new EditActionManager();
		return manager;
	}

	public static void setManager(EditActionManager manager) {
		EditActionManager.manager = manager;
	}

	public void addDropMenuAction(DropMenuAction action) {
		dropMenuActions.add(action);
	}

	public void removeDropMenuAction(DropMenuAction action) {
		dropMenuActions.remove(action);
	}

	public List<DropMenuAction> getDropMenuActions() {
		return dropMenuActions;
	}

	public void addEditAction(ClickMenuAction action) {
		editActions.add(action);
	}

	public void removeEditAction(ClickMenuAction action) {
		editActions.remove(action);
	}

	public List<ClickMenuAction> getEditActions() {
		return editActions;
	}

	public void addInputHandler(InputHandlerI handler) {
		if (defaultInputHandler == null)
			setDefaultInputHandler(handler);
		inputHandlers.add(handler);
	}

	public void removeInputHandler(InputHandlerI handler) {
		inputHandlers.remove(handler);
	}

	public InputHandlerI getInputHandler(String id) {
		for (InputHandlerI handler : inputHandlers) {
			if (handler.getID().equals(id))
				return handler;
		}
		return null;
	}

	public List<InputHandlerI> getInputHandlers() {
		return inputHandlers;
	}

	public void setDefaultInputHandler(InputHandlerI defaultInputHandler) {
		this.defaultInputHandler = defaultInputHandler;
	}

	public InputHandlerI getDefaultInputHandler() {
		return defaultInputHandler;
	}
}
