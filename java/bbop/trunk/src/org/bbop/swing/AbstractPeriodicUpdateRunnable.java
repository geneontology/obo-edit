package org.bbop.swing;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.SwingUtilities;
import javax.swing.Timer;

import org.bbop.util.TaskDelegate;

public abstract class AbstractPeriodicUpdateRunnable implements Runnable {

	protected Timer timer;

	protected BackgroundEventQueue queue;
	protected boolean swingFriendly;

	public AbstractPeriodicUpdateRunnable(BackgroundEventQueue queue,
			boolean swingFriendly) {
		this.queue = queue;
		this.swingFriendly = swingFriendly;
	}

	protected abstract void setupUpdate();

	protected abstract void cleanupUpdate();
	
	protected abstract void doUpdate(TaskDelegate<?> task);

	protected abstract boolean isCancelled();

	public void run() {
		clearTimer();
		if (swingFriendly) {
			SwingUtilities.invokeLater(new Runnable() {

				public void run() {
					setupUpdate();
				}
			});
		} else
			setupUpdate();
		timer = new Timer(100, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateStep();
			}
		});
		timer.start();
	}

	protected void clearTimer() {
		if (timer != null)
			timer.stop();
		timer = null;
		if (swingFriendly) {
			SwingUtilities.invokeLater(new Runnable() {

				public void run() {
					cleanupUpdate();
				}
			});
		} else
			cleanupUpdate();
	}

	protected void updateStep() {
		final TaskDelegate<?> currentTask = queue.getCurrentTask();
		if (currentTask == null) {
			clearTimer();
		} else {
			if (isCancelled()) {
				queue.cancelAll();
				clearTimer();
			} else {
				if (swingFriendly) {
					SwingUtilities.invokeLater(new Runnable() {

						public void run() {
							doUpdate(currentTask);
						}
					});
				} else
					doUpdate(currentTask);
			}
		}
	}

	public void install() {
		queue.addStartupNotifier(this);
	}

	public void shutdown() {
		queue.removeStartupNotifier(this);
	}
}
