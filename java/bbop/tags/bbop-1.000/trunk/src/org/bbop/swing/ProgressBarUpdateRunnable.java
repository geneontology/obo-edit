package org.bbop.swing;

import javax.swing.JProgressBar;

import org.bbop.util.TaskDelegate;

public class ProgressBarUpdateRunnable extends AbstractPeriodicUpdateRunnable {

	protected JProgressBar progressBar;
	
	public ProgressBarUpdateRunnable(BackgroundEventQueue queue,
			JProgressBar progressBar) {
		super(queue, true);
		this.progressBar = progressBar;
	}

	@Override
	protected void cleanupUpdate() {
	}

	@Override
	protected void doUpdate(TaskDelegate<?> task) {
		Number n = task.getProgressValue();
		if (n != null)
			progressBar.setValue(n.intValue());
		String s = task.getProgressString();
		if (s != null)
			progressBar.setString(s);
	}

	@Override
	protected boolean isCancelled() {
		return false;
	}

	@Override
	protected void setupUpdate() {
	}

}
