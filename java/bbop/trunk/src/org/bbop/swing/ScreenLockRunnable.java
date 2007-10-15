package org.bbop.swing;

import java.awt.Frame;

import org.bbop.util.TaskDelegate;

public class ScreenLockRunnable extends AbstractPeriodicUpdateRunnable {

	ModalProgressMonitor monitor;
	protected Frame frame;
	protected boolean modal;

	public ScreenLockRunnable(BackgroundEventQueue queue, Frame frame,
			boolean modal) {
		super(queue, true);
		this.frame = frame;
		this.modal = modal;
	}

	@Override
	protected void setupUpdate() {
		monitor = new ModalProgressMonitor(ScreenLockRunnable.this.frame, "",
				"", 0, 100, ScreenLockRunnable.this.modal);
		monitor.setMillisToPopup(100);
		monitor.setProgress(0);
	}

	@Override
	protected void cleanupUpdate() {
		if (monitor != null)
			monitor.close();
		monitor = null;
	}

	@Override
	protected boolean isCancelled() {
		return monitor != null && monitor.isCanceled();
	}

	@Override
	protected void doUpdate(TaskDelegate<?> currentTask) {
		Number n = currentTask.getProgressValue();
		if (n != null)
			monitor.setProgress(n.intValue());
		String s = currentTask.getProgressString();
		if (s != null)
			monitor.setNote(s);
	}
}
