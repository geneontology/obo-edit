package org.geneontology.web;


import java.io.IOException;

import org.apache.log4j.Logger;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphWrapper;

public class DbOperationsTask extends Task implements DbOperationsListener{

	private static Logger LOG = Logger.getLogger(DbOperationsTask.class);
	
	private String opName;
	
//	private DbOperations db;
	
	private OWLGraphWrapper graph;
	
	private boolean force;
	
	private String tablePrefix;
	
	private String tsvFileDir;
	
	public DbOperationsTask(String op, OWLGraphWrapper graph, boolean force, String tablePrefix, String tsvFilesDir){
		super();
		
		this.opName =op;
		this.graph = graph;
		this.force = force;
		this.tsvFileDir = tsvFilesDir;
	}
	
	public DbOperationsTask(String op) throws OWLOntologyCreationException, IOException{
		this(op, new DbOperations().buildOWLGraphWrapper(), false, "", "");
	}	
	@Override
	public void run(){
		LOG.info("Running Db operation : " + opName);
	
		
		this.exception = null; 
		running = true;
		DbOperations db = new DbOperations();
		db.addDbOperationsListener(this);
		try{
			if("bulkload".equals(opName)){
					db.bulkLoad(graph, force);
			}else if ("update".equals(opName)){
					db.updateGold(graph);
			}else if ("buildschema".equals(opName)){
					db.buildSchema(force, tablePrefix);
			}else if ("buildtsv".equals(opName)){
				db.dumpFiles(tablePrefix, graph);
			}else if ("loadtsv".equals(opName)){
				db.loadTsvFiles(tsvFileDir);
			}
		} catch (Exception e) {
			running = false;
			this.exception = e;
			e.printStackTrace();
			LOG.error("DB Operation failed " + opName, e);
		}finally{
			running = false;
		}

	}
	
	public String getOperationName(){
		return opName;
	}
	
	protected void reportStartTime(String name){
		this.addInProgress(name);
	}
	
	protected void reportEndTime(String name){
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


	public void startOboToOWL() {
		reportStartTime("Obo To OWL Conversion");
	}


	public void endOboToOWL() {
		reportEndTime("Obo To OWL Conversion");
		
	}
	
	
}
