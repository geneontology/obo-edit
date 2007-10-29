package org.bbop.framework;


public abstract class AbstractSingleActionTask implements GUITask, Runnable {

	protected GUIManager manager;
	
	public AbstractSingleActionTask() {
		this(null);
	}

	public AbstractSingleActionTask(GUIManager manager) {
		this.manager = manager;
	}
	
	protected GUIManager getManager() {
		if (manager == null)
			return GUIManager.getManager();
		else
			return manager;
	}

	public void install() {
		run();
		getManager().notifyComplete(this);
	}

	public void shutdown() {
	}
}
