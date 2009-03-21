package org.bbop.util;

import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;
import java.util.List;

import javax.swing.SwingUtilities;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.DataAdapterException;

public abstract class AbstractTaskDelegate<T> implements TaskDelegate<T> {
	
	protected final static Logger logger = Logger.getLogger(AbstractTaskDelegate.class);


	protected boolean cancelled = false;
	protected boolean threwException = false;
	protected boolean running = false;
	protected boolean isSwingFriendly = false;
	protected Integer progress = 0;
	protected String progressString = null;
	protected T results;
	protected Throwable exception;
	protected List<Runnable> postExecuteRunnables = new LinkedList<Runnable>();
	protected List<Runnable> cancelledRunnables = new LinkedList<Runnable>();
	protected List<Runnable> failedRunnables = new LinkedList<Runnable>();

	public T getResults() {
		return results;
	}

	protected void setResults(T results) {
		this.results = results;
	}

	public void cancel() {
		cancelled = true;
		running = false;
	}

	public String getProgressString() {
		return progressString;
	}

	public Number getProgressValue() {
		return progress;
	}

	public boolean isCancelled() {
		return cancelled;
	}

	public boolean isRunning() {
		return running;
	}

	public void run() {
		running = true;
		try {
			execute();
		} 
		catch (Exception e){
			threwException = true;
			e.printStackTrace();
			this.exception = e;
		}
		catch (Throwable t) {
			threwException = true;
			exception = t; 
			logger.error("Problem running task:", t);
		}
		
//		logger.debug("isFailed: " +  isFailed());
		if (isFailed()) {
			for (Runnable r : failedRunnables) {
				if (isSwingFriendly) {
					try {
						SwingUtilities.invokeAndWait(r);
					} catch (InterruptedException e) {
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						e.printStackTrace();
					}
				} else
					r.run();
			}
			failed();
		} else if (isCancelled()) {
			for (Runnable r : cancelledRunnables) {
				if (isSwingFriendly) {
					try {
						SwingUtilities.invokeAndWait(r);
					} catch (InterruptedException e) {
					} catch (InvocationTargetException e) {
					}
				} else
					r.run();
			}
			cancelled();
		} else {
			for (Runnable r : postExecuteRunnables) {
				if (isSwingFriendly) {
					// SwingUtilities.invokeLater(r);
					try {
						SwingUtilities.invokeAndWait(r);
					} catch (InterruptedException e) {
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						e.printStackTrace();
					}
				} else
					r.run();
			}
			done();
		}
		running = false;
	}
	
	protected void setProgressValue(Integer progress) {
		this.progress = progress;
	}
	
	protected void setProgressString(String string) {
		this.progressString = string;
	}

	protected void done() {
	}
	
	protected void cancelled() {
	}

	public void addPostExecuteRunnable(Runnable r) {
		postExecuteRunnables.add(r);
	}

	public void removePostExecuteRunnable(Runnable r) {
		postExecuteRunnables.add(r);
	}
	
	public void addCancelledRunnable(Runnable r) {
		cancelledRunnables.add(r);
	}

	public void removeCancelledRunnable(Runnable r) {
		cancelledRunnables.remove(r);
	}
	
	public void addFailedRunnable(Runnable r) {
		failedRunnables.add(r);
	}
	
	public void removeFailedRunnable(Runnable r) {
		failedRunnables.remove(r);
	}

	public abstract void execute() throws Exception;
	
	public boolean isFailed() {
		return threwException;
	}
	
	protected void failed() {
	}

	public void setSwingFriendly(boolean swingFriendly) {
		this.isSwingFriendly = swingFriendly;
	}
	
	public Throwable getException() {
		return exception;
	}
}
