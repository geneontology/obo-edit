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
//		System.err.println("!!!updating progress bar with "+n);
		if (n != null) {
			progressBar.setIndeterminate(false);
			progressBar.setValue(n.intValue());
		} else
			progressBar.setIndeterminate(true);
		String s = task.getProgressString();
		if (s != null)
			progressBar.setString(s);
		progressBar.repaint();
	}

	@Override
	protected boolean isCancelled() {
		return false;
	}

	@Override
	protected void setupUpdate() {
	}

}
