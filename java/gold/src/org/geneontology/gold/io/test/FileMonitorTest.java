package org.geneontology.gold.io.test;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.FileMonitor;
import org.geneontology.gold.io.FileMonitorListener;

import junit.framework.TestCase;

public class FileMonitorTest extends TestCase {

	public void testFileMonitor() throws InterruptedException{
		
		FileMonitorListener fileListener = new FileMonitorListener() {
			
			public void filesModified(String[] files) {
				for(String file: files){
					System.out.println("Modified File " + file);
				}
				
			}
		};
		
		FileMonitor monitor = new FileMonitor(GeneOntologyManager.getInstance().getDefaultOntologyLocations(), 1*60*100);
		monitor.addFileMonitorListener(fileListener);
		monitor.startMonitoring();
		
		Thread.sleep(5*60*1000);
		
		monitor.stopMonitoring();
		
	}
	
	
}
