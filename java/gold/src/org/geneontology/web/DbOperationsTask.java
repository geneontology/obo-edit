package org.geneontology.web;


import java.io.IOException;
import java.util.List;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphWrapper;

public class DbOperationsTask extends Task implements DbOperationsListener{

	private static Logger LOG = Logger.getLogger(DbOperationsTask.class);
	
	private String opName;
	
//	private DbOperations db;
	
	//private OWLGraphWrapper []graphs;
	
	private String[] locations;
	
	private boolean force;
	
	private String tablePrefix;
	
	private String tsvFileDir;
	
	//id of the current
	private String currentOntologyBeingProcessed;
	
	private List<OWLGraphWrapper> graphs;
	
	public DbOperationsTask(String op, String []locations, boolean force, String tablePrefix, String tsvFilesDir){
		super();
		
		this.opName =op;
		this.locations = locations;
		this.force = force;
		this.tsvFileDir = tsvFilesDir;
		graphs = new Vector<OWLGraphWrapper>();
	}
	
	public DbOperationsTask(String op) throws OWLOntologyCreationException, IOException{
		//this(op, new DbOperations().buildOWLGraphWrapper(), false, "", "");
		this(op, null, false, "", "");

	}
	@Override
	public void run(){
		LOG.info("Running Db operation : " + opName);
	
		
		this.exception = null; 
		running = true;
		DbOperations db = new DbOperations();
		db.addDbOperationsListener(this);
		
		try{
			for(String location: locations){
				this.currentOntologyBeingProcessed = location;
				if("bulkload".equals(opName)){
						db.bulkLoad(location, force);
				}else if ("update".equals(opName)){
						db.updateGold(location);
				}else if ("buildschema".equals(opName)){
						db.buildSchema(force, tablePrefix);
				}else if ("buildtsv".equals(opName)){
					db.dumpFiles(tablePrefix, location);
				}else if ("loadtsv".equals(opName)){
					db.loadTsvFiles(tsvFileDir);
				}
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
		reportStartTime("BulkLoad/TotalTime--"+currentOntologyBeingProcessed);
	}

	public void bulkLoadEnd() {
		reportEndTime("BulkLoad/TotalTime--"+currentOntologyBeingProcessed);
	}

	public void dumpFilesStart() {
		reportStartTime("DumpFiles--"+currentOntologyBeingProcessed);
	}

	public void dumpFilesEnd() {
		reportEndTime("DumpFiles--"+currentOntologyBeingProcessed);

	}

	public void buildSchemaStart() {
		reportStartTime("BuildSchema--"+currentOntologyBeingProcessed);
	}

	public void buildSchemaEnd() {
		reportEndTime("BuildSchema--"+currentOntologyBeingProcessed);
	}

	public void loadTsvFilesStart() {
		reportStartTime("LoadTsvFiles--"+currentOntologyBeingProcessed);
	}

	public void loadTsvFilesEnd() {
		reportEndTime("LoadTsvFiles--"+currentOntologyBeingProcessed);
	}

	public void updateStart() {
		reportStartTime("Update/TotalTime--"+currentOntologyBeingProcessed);
	}

	public void updateEnd() {
		reportEndTime("Update/TotalTime--"+currentOntologyBeingProcessed);
	}


	public void startOntologyLoad() {
		reportStartTime("Obo To OWL Conversion--"+currentOntologyBeingProcessed);
	}


	public void endOntologyLoad(OWLGraphWrapper graph) {
		reportEndTime("Obo To OWL Conversion--"+currentOntologyBeingProcessed);
		graphs.add(graph);
	}
	
	public List<OWLGraphWrapper> getGraphs(){
		return graphs;
	}
	
}
