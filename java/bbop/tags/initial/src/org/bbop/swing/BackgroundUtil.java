package org.bbop.swing;

import org.bbop.util.TaskDelegate;

public class BackgroundUtil {

	private BackgroundUtil() {
	}
	
	public static void scheduleTask(TaskDelegate<?> task) {
		BackgroundEventQueue.getGlobalQueue().scheduleTask(task);
	}
	
	public static void scheduleTasks(TaskDelegate<?>... tasks) {
		BackgroundEventQueue.getGlobalQueue().scheduleTasks(tasks);
	}

	public static void scheduleDependentTasks(TaskDelegate<?>... tasks) {
		BackgroundEventQueue.getGlobalQueue().scheduleDependentTasks(tasks);
	}

	/*
	public static void runInBackground(final Runnable task,
			final Progressible p, final Runnable finishedTask,
			final Window frame, final String title, final String desc,
			final int updateInterval, final boolean isModal) {
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

			protected String desc;

			protected int progress;

			@Override
			protected Void doInBackground() throws Exception {
				ModalProgressMonitor pm = null;

				ProgressListener listener = null;
				Timer timer = null;

				if (p != null) {
					pm = new ModalProgressMonitor(frame, title, desc, 0, 100,
							isModal);
					pm.setMillisToDecideToPopup(0);
					pm.setMillisToPopup(0);
					listener = new ProgressListener() {
						public void progressMade(ProgressEvent e) {
							if (e.getDescription() != null)
								desc = e.getDescription();
							if (e instanceof ReusableProgressEvent) {
								progress = ((ReusableProgressEvent) e)
										.getFastVal();
							} else
								progress = e.getValue().intValue();
						}
					};
					p.addProgressListener(listener);
					final ModalProgressMonitor finalpm = pm;
					timer = new Timer(updateInterval, new ActionListener() {

						public void actionPerformed(ActionEvent e) {
							Runnable r = new Runnable() {
								public void run() {
									finalpm.setProgress(progress);
									if (desc != null) {
										finalpm.setNote(desc);
									}
								};
							};
							SwingUtilities.invokeLater(r);
						}

					});
				}
				if (timer != null)
					timer.start();
				task.run();
				if (p != null && listener != null)
					p.removeProgressListener(listener);
				if (timer != null)
					timer.stop();
				if (pm != null)
					pm.close();
				if (finishedTask != null)
					SwingUtilities.invokeLater(finishedTask);
				return null;
			}
		};
		worker.execute();
	}
	*/
}
