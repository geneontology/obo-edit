package org.geneontology.web;


import org.apache.log4j.Logger;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;

public class DbOperationsTask extends Task implements DbOperationsListener{

	private static Logger LOG = Logger.getLogger(DbOperationsTask.class);
	
	private String opName;
	
	public DbOperationsTask(String op){
		super();
		this.opName =op;
	}
	
	
	@Override
	public void run(){
		LOG.info("Running Db operation : " + opName);
	
		this.exception = null; 
		running = true;
		if("bulkload".equals(opName)){
			DbOperations db = new DbOperations();
			db.addDbOperationsListener(this);
			try {
				db.bulkLoad(false);
			} catch (Exception e) {
				running = false;
				this.exception = e;
				e.printStackTrace();
				LOG.error("DB Operation failed " + opName, e);
			}finally{
				running = false;
			}
		}else if ("update".equals(opName)){
			DbOperations db = new DbOperations();
			db.addDbOperationsListener(this);
			try {
				db.updateGold();
			} catch (Exception e) {
				running = false;
				this.exception = e;
				e.printStackTrace();
				LOG.error("DB Operation failed " + opName, e);
			}finally{
				running = false;
			}
			
		}
		running = false;
	}
	
	public String getOperationName(){
		return opName;
	}
	
	private void reportStartTime(String name){
		this.addInProgress(name);
	}
	
	private void reportEndTime(String name){
		this.addCompleted(name);
	}
	
	public void bulkLoadStart() {
		reportStartTime("BulkLoad/TotalTime");
	}

	public void bulkLoadEnd() {
		reportEndTime("BulkLoad/TotalTime");
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
		reportStartTime("Update/TotalTime");
	}

	public void updateEnd() {
		reportEndTime("Update/TotalTime");
	}
	
	
}
