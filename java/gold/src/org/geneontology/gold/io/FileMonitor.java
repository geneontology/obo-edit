package org.geneontology.gold.io;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Hashtable;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.apache.log4j.Logger;

/**
 * This is monitor runs in background to check whether files is modified or not. If a file is modified
 * then it call it abstract method 'modified' with the file path in parameter. 
 * @author Shahid Manzoor
 *
 */
public class FileMonitor extends TimerTask {

	private static Logger LOG = Logger.getLogger(FileMonitor.class);
	
	private Timer timer;
	
	//delay in milliseconds
	private long delay;
	
	
	private List<String> modifiedFiles;
	
	private Hashtable<String, Long> table;
	
	private List filesToMonitor;
	
	private List<FileMonitorListener> listeners;
	/**
	 * 
	 * @param files List of files paths to be monitored. 
	 * @param delay Time in milliseconds to check whether the files are modified.
	 */
	public FileMonitor(List files, long delay){
		this.delay = delay;
		timer = new Timer();
		table = new Hashtable<String, Long>();
		modifiedFiles = new ArrayList<String>();
		this.filesToMonitor = files;
		listeners = new ArrayList<FileMonitorListener>();
		try{
			for(Object location: files){
				long time = getLastModified(location.toString());
				
				table.put(location.toString(), time);
			}
		}catch(Exception ex){
			LOG.error(ex);
		}
	}
	
	public void addFileMonitorListener(FileMonitorListener listener){
		listeners.add(listener);
	}
	
	private long getLastModified(String location) throws IOException{
		URL url = null;
		if(location.startsWith("http://"))
			url = new URL(location);
		else{
			File f = new File(location);
			url = f.toURI().toURL();
		}
		
		
		URLConnection con = url.openConnection();

		return con.getLastModified();
	}
	
	private boolean isModified(String location) throws IOException{
		
		long currentTime = getLastModified(location);
		long lastTime = table.get(location);
		
		if(currentTime != lastTime){
	//		modifiedFiles.add(location);
			table.put(location, currentTime);
			return true;
		}
		
		return false;
	}
	
	@Override
	public void run() {
		try{
			modifiedFiles.clear();
			for(Object location: this.filesToMonitor){
				if(isModified(location.toString())){
					modifiedFiles.add(location.toString());
					//modified(location.toString());
				}
			}

			if(!modifiedFiles.isEmpty()){
				String files[] = new String[modifiedFiles.size()];
				modifiedFiles.toArray(files);
				for(FileMonitorListener listener: listeners){
					listener.filesModified(files);
				}
			}
			
		//	modified(modifiedFiles);
		}catch(Exception ex){
			LOG.error(ex, ex);
		}
		
		
	}
	
	public void startMonitoring(){
		//timer.schedule(this, delay);
		timer.scheduleAtFixedRate(this, Calendar.getInstance().getTime(), delay);
	}
	
	public void stopMonitoring(){
		timer.cancel();
	}


	public long getDelay() {
		return delay;
	}

	/**
	 * 
	 * @param delay in milliseconds
	 */
	public void setDelay(long delay) {
		this.delay = delay;
	}
	
}
