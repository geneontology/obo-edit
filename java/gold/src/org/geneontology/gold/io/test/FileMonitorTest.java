package org.geneontology.gold.io.test;

import javax.swing.JOptionPane;

import org.geneontology.gold.io.FileMonitor;

import junit.framework.TestCase;

public class FileMonitorTest extends TestCase {

	public void testFileMonitor() throws InterruptedException{
		
		FileMonitor monitor = new FileMonitor();
		monitor.setDelay(1*60*1000);
		
		monitor.startMonitoring();
		
		Thread.sleep(5*60*1000);
		
	//	JOptionPane.showMessageDialog(null, "Stop this");
		monitor.stopMonitoring();
		
	}
	
	
}
