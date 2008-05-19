package org.bbop.util;

import java.util.TimerTask;

import org.apache.log4j.*;

public class RunnableTimerTask extends TimerTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RunnableTimerTask.class);

	protected Runnable runnable;
	
	public RunnableTimerTask(Runnable runnable) {
		this.runnable = runnable;
	}
	
	@Override
	public void run() {
		runnable.run();
	}

}
