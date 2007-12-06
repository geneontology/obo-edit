package org.bbop.swing;

import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.JLabel;

import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.TaskDelegate;

public class BackgroundUtil {

	private BackgroundUtil() {
	}

	public static void scheduleTask(TaskDelegate<?> task) {
		scheduleTasks(task);
	}

	public static void scheduleTask(BackgroundEventQueue queue,
			TaskDelegate<?> task, boolean block, String blockMessage) {
		scheduleTasks(queue, block, blockMessage, task);
	}

	public static void scheduleTasks(TaskDelegate<?>... tasks) {
		scheduleTasks(BackgroundEventQueue.getGlobalQueue(), false, null, tasks);
	}

	public static void scheduleTasks(BackgroundEventQueue queue, boolean block,
			String blockMessage, TaskDelegate<?>... tasks) {
		JDialog dialog = null;
		if (block) {
			dialog = new JDialog((Frame) null, true);
			tasks = getBlockingTasks(tasks, dialog, blockMessage);
		}
		queue.scheduleTasks(tasks);
		if (block) {
			block(dialog);
		}
	}

	protected static TaskDelegate<?>[] getBlockingTasks(
			TaskDelegate<?>[] tasks, JDialog dialog, String blockMessage) {
		dialog.setTitle(blockMessage);
		dialog.getContentPane().add(new JLabel(blockMessage));
		dialog.setLocation(0, 0);
		dialog.toBack();
		TaskDelegate<?>[] newTasks = new TaskDelegate<?>[tasks.length + 1];
		for (int i = 0; i < tasks.length; i++)
			newTasks[i] = tasks[i];
		final JDialog fdialog = dialog;
		AbstractTaskDelegate finishTask = new AbstractTaskDelegate<Void>() {

			@Override
			public void execute() throws Exception {
			}
		};
		finishTask.setSwingFriendly(true);
		Runnable post = new Runnable() {

			public void run() {
				fdialog.setVisible(false);
				fdialog.dispose();
			}

		};
		finishTask.addPostExecuteRunnable(post);
		finishTask.addCancelledRunnable(post);
		finishTask.addFailedRunnable(post);
		newTasks[tasks.length] = finishTask;
		return newTasks;
	}

	protected static void block(JDialog dialog) {
		dialog.pack();
		dialog.toBack();
		dialog.setVisible(true);
	}

	public static void scheduleDependentTasks(TaskDelegate<?>... tasks) {
		scheduleDependentTasks(BackgroundEventQueue.getGlobalQueue(), false,
				null, tasks);
	}

	public static void scheduleDependentTasks(BackgroundEventQueue queue,
			boolean block, String blockMessage, TaskDelegate<?>... tasks) {
		queue.scheduleDependentTasks(tasks);
	}

	/*
	 * public static void runInBackground(final Runnable task, final
	 * Progressible p, final Runnable finishedTask, final Window frame, final
	 * String title, final String desc, final int updateInterval, final boolean
	 * isModal) { SwingWorker<Void, Void> worker = new SwingWorker<Void,
	 * Void>() {
	 * 
	 * protected String desc;
	 * 
	 * protected int progress;
	 * 
	 * @Override protected Void doInBackground() throws Exception {
	 * ModalProgressMonitor pm = null;
	 * 
	 * ProgressListener listener = null; Timer timer = null;
	 * 
	 * if (p != null) { pm = new ModalProgressMonitor(frame, title, desc, 0,
	 * 100, isModal); pm.setMillisToDecideToPopup(0); pm.setMillisToPopup(0);
	 * listener = new ProgressListener() { public void
	 * progressMade(ProgressEvent e) { if (e.getDescription() != null) desc =
	 * e.getDescription(); if (e instanceof ReusableProgressEvent) { progress =
	 * ((ReusableProgressEvent) e) .getFastVal(); } else progress =
	 * e.getValue().intValue(); } }; p.addProgressListener(listener); final
	 * ModalProgressMonitor finalpm = pm; timer = new Timer(updateInterval, new
	 * ActionListener() {
	 * 
	 * public void actionPerformed(ActionEvent e) { Runnable r = new Runnable() {
	 * public void run() { finalpm.setProgress(progress); if (desc != null) {
	 * finalpm.setNote(desc); } }; }; SwingUtilities.invokeLater(r); }
	 * 
	 * }); } if (timer != null) timer.start(); task.run(); if (p != null &&
	 * listener != null) p.removeProgressListener(listener); if (timer != null)
	 * timer.stop(); if (pm != null) pm.close(); if (finishedTask != null)
	 * SwingUtilities.invokeLater(finishedTask); return null; } };
	 * worker.execute(); }
	 */
}
