package org.geneontology.web;

public class TaskRunner extends Thread implements TaskExecutionListener {

	protected boolean running;

	protected Object data;
	
	protected TaskExecution task;
	
	public TaskRunner(TaskExecution task){
		
		running = true;
		this.task = task;
		task.addTaskExecutionListener(this);
		
		
	}
	
	public Object getData(){
		return this.data;
	}
	

	public void run(){
		
		task.execute();
		
		running = false;
	}
	
	public boolean isRunning(){
		return running;
	}

	public void updateData(Object data) {
		this.data = data;
	}
	
	
}
