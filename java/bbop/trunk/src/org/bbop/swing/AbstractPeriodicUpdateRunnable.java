package org.bbop.swing;

import javax.swing.SwingUtilities;

import java.util.Timer;
import java.util.TimerTask;

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
//			try {
//				SwingUtilities.invokeAndWait(new Runnable() {
//
//					public void run() {
//						setupUpdate();
//					}
//				});
//			} catch (InterruptedException e) {
//			} catch (InvocationTargetException e) {
//			}
		} else
			setupUpdate();
		
		timer = new Timer(true);
		timer.scheduleAtFixedRate(new TimerTask() {

			@Override
			public void run() {
				updateStep();
			}
			
		}, 100, 200);
	}

	protected void clearTimer() {
		if (timer != null)
			timer.cancel();
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
	
	protected TaskDelegate<?> getCurrentTask() {
		return queue.getCurrentTask();
	}

	protected void updateStep() {
		final TaskDelegate<?> currentTask = getCurrentTask();
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
//					try {
//						SwingUtilities.invokeAndWait(new Runnable() {
//
//							public void run() {
//								doUpdate(currentTask);
//							}
//						});
//					} catch (InterruptedException e) {
//					} catch (InvocationTargetException e) {
//					}
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
