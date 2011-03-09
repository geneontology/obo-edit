package org.geneontology.web;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Hashtable;

public abstract class Task extends Thread {

	protected boolean running;
	
	private Hashtable<String, long[]> completedOperations;
	
	private Collection<String> opsOrder;
	
	protected String currentRunningOperation;
	
	protected Exception exception;
	
	protected Object data;
	
	public Task(){
		completedOperations = new Hashtable<String, long[]>();
		opsOrder = new ArrayList<String>();
		running = true;
	}
	
	
	public Exception getException(){
		return this.exception;
	}
	
	public boolean isRunning(){
		return running;
	}
	
	public Collection<String> getCompletedOperations(){
		//return completedOperations.keySet();
		return opsOrder;
	}

	
	
	public String getCurrentRunningOperation(){
		return this.currentRunningOperation;
	}

	protected void addInProgress(String opName){
		currentRunningOperation = opName;
		completedOperations.put(opName, new long[]{Calendar.getInstance().getTimeInMillis(),0});
		opsOrder.add(opName);
	}
	
	protected boolean addCompleted(String opName){
		long[] t = completedOperations.get(opName);
		
		if(t != null){
			t[1] = Calendar.getInstance().getTimeInMillis();
			return true;
		}
		
		return false;
	}
	
	public long getStartTime(String opName){
		long[] t = completedOperations.get(opName);
		
		if(t != null){
			return t[0];
		}
		
		return -1;
	}

	public long getEndTime(String opName){
		long[] t = completedOperations.get(opName);
		
		if(t != null){
			return t[1];
		}
		
		return -1;
	}
	
	public void run(){
		
		execute();
		
		running = false;
	}
	
	public Object getData(){
		return data;
	}
	
	
	//sub classes must implement this method
	public abstract void execute();
	
}
