package org.geneontology.gold.io;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Hashtable;

import org.apache.log4j.Logger;
import org.geneontology.web.DbOperationsTask;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphWrapper;

public class DbOperationsListenerToReportTime extends DbOperationsTask {

	public DbOperationsListenerToReportTime(String op) throws OWLOntologyCreationException, IOException {
		super(op);
	}

	
	
	public DbOperationsListenerToReportTime(String op, String locations[],
			boolean force, String tablePrefix, String tsvFilesDir) {
		super(op, locations, force, tablePrefix, tsvFilesDir);
	}



	private static Logger LOG = Logger.getLogger(DbOperationsListenerToReportTime.class);

	private static final SimpleDateFormat dtFormat = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");
	
	private Hashtable<String, Date> timeLogs = new Hashtable<String, Date>();
	
	protected void reportStartTime(String name){
		Date dt = Calendar.getInstance().getTime();
		timeLogs.put(name, dt);
		
		System.out.println(name + " - start time\t\t" + dtFormat.format(dt));
		
	}
	
	protected void reportEndTime(String name){
		Date dt = Calendar.getInstance().getTime();
		System.out.println(name + " - end time\t\t" + dtFormat.format(dt));

		Date stDt = timeLogs.get(name);

		float timeTaken = (float)(dt.getTime() - stDt.getTime())/1000;
		
		System.out.println( name + " - total time taken in seconds:\t\t" + timeTaken);
		
	}
	
}
