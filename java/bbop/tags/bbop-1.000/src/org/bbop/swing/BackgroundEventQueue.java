package org.bbop.swing;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.TaskDelegate;

public class BackgroundEventQueue {
	
	private static int idgen = 0;
	private int id = idgen++;

	protected static class TaskGroup extends AbstractTaskDelegate<Void> {

		protected TaskDelegate<?>[] tasks;

		public TaskGroup(TaskDelegate<?>[] tasks) {
			this.tasks = tasks;
		}

		public TaskDelegate<?>[] getTasks() {
			return tasks;
		}

		@Override
		public void execute() {
		}

		public void cancel() {
			super.cancel();
			for (TaskDelegate<?> t : getTasks()) {
				t.cancel();
			}
		}

	}

	protected static class BackgroundEventThread extends Thread {

		protected LinkedList<TaskDelegate<?>> l = new LinkedList<TaskDelegate<?>>();
		protected TaskDelegate<?> currentTask;
		protected List<Runnable> startupNotifiers = new LinkedList<Runnable>();
		protected BackgroundEventQueue queue;
		protected boolean kill = false;

		public BackgroundEventThread(BackgroundEventQueue queue) {
			setDaemon(true);
			this.queue = queue;
		}
		
		public void kill() {
			kill = true;
			interrupt();
		}

		public void run() {
			while (!kill) {
				try {
					join();
				} catch (InterruptedException e) {
				}
				TaskDelegate<?> t;

				for (Runnable r : startupNotifiers) {
					r.run();
				}
				while ((t = l.poll()) != null) {
					executeTask(t);
				}
				currentTask = null;
			}
		}
		
		public void cancelAll() {
			List<TaskDelegate<?>> cancelUs = new LinkedList<TaskDelegate<?>>(l);
			if (currentTask != null)
				cancelUs.add(0, currentTask);
			for(TaskDelegate<?> t : cancelUs) {
				t.cancel();
			}
			
		}

		protected void addStartupNotifier(Runnable r) {
			startupNotifiers.add(r);
		}

		protected void removeStartupNotifier(Runnable r) {
			startupNotifiers.remove(r);
		}

		protected boolean executeTask(TaskGroup g) {
			for (TaskDelegate<?> t : g.getTasks()) {
				boolean cancelled = executeTask(t);
				if (cancelled) {
					g.cancel();
					return true;
				}
			}
			return false;
		}

		protected boolean executeTask(TaskDelegate<?> t) {
			if (t instanceof TaskGroup)
				return executeTask((TaskGroup) t);
			else {
				try {
					if (t.isCancelled())
						return true;
					currentTask = t;
					currentTask.run();
					return currentTask.isCancelled();
				} catch (Throwable error) {
					return true;
				}
			}
		}

		protected void scheduleTask(TaskDelegate<?> t) {
			l.add(t);
			interrupt();
		}

		protected TaskDelegate<?> getCurrentTask() {
			return currentTask;
		}

		protected LinkedList<TaskDelegate<?>> getTaskQueue() {
			return l;
		}
	}

	protected BackgroundEventThread thread;

	protected static BackgroundEventQueue queue;

	public BackgroundEventQueue() {
	}
	
	public void cancelAll() {
		getBackgroundEventThread().cancelAll();
	}
	
	public void addStartupNotifier(Runnable r) {
		getBackgroundEventThread().addStartupNotifier(r);
	}

	public void removeStartupNotifier(Runnable r) {
		getBackgroundEventThread().removeStartupNotifier(r);
	}

	public static BackgroundEventQueue getGlobalQueue() {
		if (queue == null)
			queue = new BackgroundEventQueue();
		return queue;
	}

	public void scheduleTask(TaskDelegate<?> t) {
		BackgroundEventThread thread = getBackgroundEventThread();
		thread.scheduleTask(t);
	}
	
	public void scheduleTasks(TaskDelegate<?>... tasks) {
		BackgroundEventThread thread = getBackgroundEventThread();
		for(TaskDelegate<?> task : tasks) {
			thread.scheduleTask(task);
		}
	}
	
	public void scheduleDependentTasks(TaskDelegate<?>... tasks) {
		BackgroundEventThread thread = getBackgroundEventThread();
		thread.scheduleTask(new TaskGroup(tasks));
	}

	protected BackgroundEventThread getBackgroundEventThread() {
		if (thread == null) {
			thread = new BackgroundEventThread(this);
			thread.start();
		}
		return thread;
	}

	public TaskDelegate<?> getCurrentTask() {
		return thread.getCurrentTask();
	}

	public int getPendingTaskCount() {
		return thread.getTaskQueue().size();
	}
	
	@Override
	protected void finalize() throws Throwable {
		if (thread != null)
			thread.kill();
		super.finalize();
	}
	
	@Override
	public String toString() {
		return "Background Event Queue "+id;
	}

}
