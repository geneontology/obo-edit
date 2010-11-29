package org.geneontology.gold.io;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Hashtable;

import org.apache.log4j.Logger;

public class DbOperationsListenerToReportTime implements DbOperationsListener {

	private static Logger LOG = Logger.getLogger(DbOperationsListenerToReportTime.class);

	private static final SimpleDateFormat dtFormat = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");
	
	private Hashtable<String, Date> timeLogs = new Hashtable<String, Date>();
	
	private void reportStartTime(String name){
		Date dt = Calendar.getInstance().getTime();
		timeLogs.put(name, dt);
		
		System.out.println(name + " - start time\t\t" + dtFormat.format(dt));
		
	}
	
	private void reportEndTime(String name){
		Date dt = Calendar.getInstance().getTime();
		System.out.println(name + " - end time\t\t" + dtFormat.format(dt));

		Date stDt = timeLogs.get(name);

		float timeTaken = (float)(dt.getTime() - stDt.getTime())/1000;
		
		System.out.println( name + " - total time taken in seconds:\t\t" + timeTaken);
		
	}
	
	public void bulkLoadStart() {
		reportStartTime("BulkLoad");
	}

	public void bulkLoadEnd() {
		reportEndTime("BulkLoad");
	}

	public void dumpFilesStart() {
		reportStartTime("DumpFiles");
	}

	public void dumpFilesEnd() {
		reportEndTime("DumpFiles");

	}

	public void buildSchemaStart() {
		reportStartTime("BuildSchema");
	}

	public void buildSchemaEnd() {
		reportEndTime("BuildSchema");
	}

	public void loadTsvFilesStart() {
		reportStartTime("LoadTsvFiles");
	}

	public void loadTsvFilesEnd() {
		reportEndTime("LoadTsvFiles");
	}

	public void updateStart() {
		reportStartTime("Update");
	}

	public void updateEnd() {
		reportEndTime("Update");
	}

}
