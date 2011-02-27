package org.geneontology.gold.io;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;

public class FileMonitor extends TimerTask {

	private static Logger LOG = Logger.getLogger(FileMonitor.class);
	
	private Timer timer;
	
	//delay in milliseconds
	private long delay;
	
	private HashSet<String> modifiedFiles;
	
	private Hashtable<String, Long> table;
	
	public FileMonitor(){
		//default delay 15 minutes;
		delay = 15 * 60 * 1000;
		timer = new Timer();
		table = new Hashtable<String, Long>();
		modifiedFiles = new HashSet<String>();
		try{
			for(Object location: GeneOntologyManager.getInstance().getDefaultOntologyLocations()){
				long time = getLastModified(location.toString());
				
				table.put(location.toString(), time);
			}
		}catch(Exception ex){
			LOG.error(ex);
		}
		
	
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
			modifiedFiles.add(location);
			table.put(location, currentTime);
			return true;
		}
		
		/*URL url = null;
		if(location.startsWith("http://"))
			url = new URL(location);
		else{
			File f = new File(location);
			url = f.toURI().toURL();
		}
		
		
		URLConnection con = url.openConnection();
		
		*/
		
		return false;
	}
	
	@Override
	public void run() {
		System.out.println("File check initiated. The current modified files are; " + this.modifiedFiles);

		try{
			for(Object location: GeneOntologyManager.getInstance().getDefaultOntologyLocations()){
				if(isModified(location.toString())){
					System.out.println("Files are modified '" + location + "' is modified");
				}
			}
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


	public void setDelay(long delay) {
		this.delay = delay;
	}
	
	

}
