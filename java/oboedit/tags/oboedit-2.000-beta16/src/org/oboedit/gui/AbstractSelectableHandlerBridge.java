package org.oboedit.gui;

import java.util.Collections;
import java.util.List;

public abstract class AbstractSelectableHandlerBridge extends
		AbstractInputHandlerBridge {

	protected boolean revertToDefaultAction;
	protected InputHandlerI defaultInputHandler;

	@Override
	protected List<InputHandlerI> getAllDragInputHandlers() {
		return Collections.emptyList();
	}

	public boolean isRevertToDefaultAction() {
		return revertToDefaultAction;
	}

	public void setRevertToDefaultAction(boolean revertToDefaultAction) {
		this.revertToDefaultAction = revertToDefaultAction;
	}

	@Override
	public InputHandlerI getDefaultInputHandler() {
		return defaultInputHandler;
	}

	public void setDefaultInputHandler(InputHandlerI defaultInputHandler) {
		this.defaultInputHandler = defaultInputHandler;
	}

}
