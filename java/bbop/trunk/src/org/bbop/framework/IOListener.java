package org.bbop.framework;

import java.util.EventListener;

public interface IOListener extends EventListener {
	public boolean willExecuteOperation(IOEvent<?> e);
	public void operationExecuted(IOEvent<?> e);
}
