package org.geneontology.web;

public interface TaskExecution {

	public void execute();
	
	public Object getData();
	
	public void addTaskExecutionListener(TaskExecutionListener listener);
	
}
