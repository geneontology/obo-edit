package org.bbop.swing;

import javax.swing.JProgressBar;

import org.bbop.util.TaskDelegate;

import org.apache.log4j.*;

public class ProgressBarUpdateRunnable extends AbstractPeriodicUpdateRunnable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ProgressBarUpdateRunnable.class);

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
//		logger.info("!!!updating progress bar with "+n);
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
