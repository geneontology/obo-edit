package org.geneontology.web.services;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

/**
 * This class runs operations asynchronously. This class maintains the  data structure to hold the information of start time and end time 
 * of sub-operations in an operation.  
 * @author Shahid Manzoor
 *
 */
public abstract class Task extends Thread {

	protected boolean running;
	
	private Hashtable<String, long[]> completedOperations;
	
	private Collection<String> opsOrder;
	
	protected String currentRunningOperation;
	
//	protected Exception exception;
	protected List<Throwable> exceptions;
	
	protected Object data;
	
	private long taskCompletionTime;
	
	public Task(){
		completedOperations = new Hashtable<String, long[]>();
		opsOrder = new ArrayList<String>();
		running = true;
		taskCompletionTime = -1;
		exceptions =new Vector<Throwable>();
	}
	
	
//	public Exception getException(){
	public List<Throwable> getException(){
		return this.exceptions;
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
		if(!opsOrder.contains(opName))
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
		taskCompletionTime = System.currentTimeMillis();
	}
	
	public Object getData(){
		return data;
	}
	
	
	public long getTaskCompletionTime(){
		return taskCompletionTime;
	}
	
	//sub classes must implement this method
	public abstract void execute();
	
}
