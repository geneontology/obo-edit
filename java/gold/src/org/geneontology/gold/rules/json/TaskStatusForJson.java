package org.geneontology.gold.rules.json;

public class TaskStatusForJson {

	private String taskName;
	private long startTime;
	private long endTime;
	private int taskId;
	
	public TaskStatusForJson(String taskName, long startTime, long endTime) {
		super();
		this.taskName = taskName;
		this.startTime = startTime;
		this.endTime = endTime;
		setTaskName(taskName);
	}

	public String getTaskName() {
		return taskName;
	}

	public long getStartTime() {
		return startTime;
	}

	public long getEndTime() {
		return endTime;
	}

	public void setTaskName(String taskName) {
		this.taskName = taskName;
		taskId = taskName != null ? taskName.hashCode() : 1;
	}

	public void setStartTime(long startTime) {
		this.startTime = startTime;
	}

	public void setEndTime(long endTime) {
		this.endTime = endTime;
	}
	
	public int getTaskId(){
		return taskId;
	}
	
	
}
