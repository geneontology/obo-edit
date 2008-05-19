package org.bbop.util;

import javax.swing.SwingUtilities;

import org.apache.log4j.*;

public class SwingUpdateTask<T> implements TaskDelegate<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SwingUpdateTask.class);

	protected TaskDelegate<T> task;
	protected Runnable swingUpdate;

	public SwingUpdateTask(TaskDelegate<T> task, Runnable swingUpdate) {
		this.task = task;
		this.swingUpdate = swingUpdate;
	}

	public void cancel() {
		task.cancel();
	}

	public T getResults() {
		return task.getResults();
	}

	public boolean isCancelled() {
		return task.isCancelled();
	}

	public boolean isRunning() {
		return task.isRunning();
	}

	public void run() {
		task.run();
		SwingUtilities.invokeLater(swingUpdate);
	}

	public String getProgressString() {
		return task.getProgressString();
	}

	public Number getProgressValue() {
		return task.getProgressValue();
	}

	public Throwable getException() {
		return null;
	}

	public boolean isFailed() {
		return false;
	}
}
