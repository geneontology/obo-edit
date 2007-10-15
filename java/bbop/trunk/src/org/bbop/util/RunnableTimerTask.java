package org.bbop.util;

import java.util.TimerTask;

public class RunnableTimerTask extends TimerTask {

	protected Runnable runnable;
	
	public RunnableTimerTask(Runnable runnable) {
		this.runnable = runnable;
	}
	
	@Override
	public void run() {
		runnable.run();
	}

}
