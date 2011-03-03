package org.geneontology.web.services;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.geneontology.web.Task;
import org.geneontology.web.TaskExecution;
import org.geneontology.web.TaskExecutionListener;
import org.geneontology.web.TaskRunner;
import org.semanticweb.owlapi.model.OWLOntology;

import owltools.graph.OWLGraphWrapper;


/**
 * This service handles gold db operations such as update and bulkload.
 *  
 * @author shaid
 *
 */
public class GoldDbOperationsService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(GoldDbOperationsService.class);
	
	
	//this instance can be share with the other service 
	private OWLGraphWrapper ontologyGraph;
	
	private String viewPath;
	
	private List<String> ontLocations;
	
	private TaskRunner runner;
	
	private boolean force;
	
	public GoldDbOperationsService(){
		
		
		buildOWLGraphWrapper(null);
		
		viewPath = "/servicesui/golddb.jsp";
		
		this.ontLocations = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
		runner = null;
	}
	
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		this.force = false;
		String command = request.getParameter("command");
		
		if(runner == null){
		
			if ("update".equals(command)) {
	
				String ontologylocation = request
						.getParameter("ontologylocation");
				
				
				if(ontologylocation != null){
					ontLocations = new ArrayList<String>();
					ontLocations.add(ontologylocation);
					runner = new TaskRunner(new GoldDbTaskExecution());
				}else{
					this.viewPath = "/servicesui/golddb-updateform.jsp";
				}
			}else if("bulkload".equals(command)){
				this.force = "true".equals(request.getParameter("force"));
				runner = new TaskRunner(new GoldDbTaskExecution());
			}
		

			if(this.runner != null){
				ontologyGraph = null;
				runner.start();
			}
		}
		
		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		
		if(runner != null){
			request.setAttribute("task", runner.getData());
		}
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}
		
		
	}
	
	public OWLGraphWrapper getGraphWrapper(){
		return this.ontologyGraph;
	}

	@Override
	public String getServiceName() {
		// TODO Auto-generated method stub
		return "gold-db-operations";
	}

	public String getViewPath(){
		return this.viewPath;
	}
	
	private void buildOWLGraphWrapper(String exclude){
		
		DbOperations db = new DbOperations();
		List ontologies = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
		OWLGraphWrapper wrapper = null;
		for(int i=0;i<ontologies.size();i++){
			
			String location = ontologies.get(i).toString();


			if(location.equals(exclude))
				continue;
			
			try{
				OWLOntology ontology = db.buildOWLOntology(location);
				if(wrapper==null){
					wrapper = new OWLGraphWrapper(ontology);
					wrapper.setSourceOntology(ontology);
				}else{
					wrapper.addSupportOntology(ontology);
				}
				
				
				for(OWLOntology sont: wrapper.getSupportOntologySet()){
					wrapper.mergeOntology(sont);
				}
				
				if(ontologyGraph == null)
					ontologyGraph = wrapper;
				//graphs.put(wrapper.getOntologyId(), wrapper); 
			}catch(Exception ex){
				LOG.error(ex.getMessage(), ex);
			}
		}
		
		
		
	}
	
	
	class GoldDbTaskExecution extends Task implements TaskExecution, DbOperationsListener{

		private List<TaskExecutionListener> listeners;

		// id of the current
		private String currentOntologyBeingProcessed;
		
		private String command;
		
	//	private Exception exception;
		
		public GoldDbTaskExecution(){
			listeners = new ArrayList<TaskExecutionListener>();
		}
		
		
		@Override
		public void execute() {
			// TODO Auto-generated method stub
		
		
			for(TaskExecutionListener l: listeners){
				l.updateData(this);
			}
			
			DbOperations db = new DbOperations();
			db.addDbOperationsListener(this);
			
			try{
				for(String ontLocation: ontLocations){
					this.currentOntologyBeingProcessed = ontLocation;
					OWLGraphWrapper graph = db.buildOWLGraphWrapper(ontLocation);
					db.bulkLoad(graph, force);
				}
				if("update".equals(command)){
					//rebuilt the merge because of change in one of the ontology
					if(GeneOntologyManager.getInstance().getDefaultOntologyLocations().size()>0)
						buildOWLGraphWrapper(ontLocations.get(0));
				}
			}catch(Exception ex){
				this.exception = ex;
				LOG.error(ex, ex);
			}
			
		}

		@Override
		public Object getData() {
			return this;
		}

		@Override
		public void addTaskExecutionListener(TaskExecutionListener listener) {
			listeners.add(listener);
		}


		protected void reportStartTime(String name) {
			this.addInProgress(name);
		}

		protected void reportEndTime(String name) {
			this.addCompleted(name);
		}

		public void bulkLoadStart() {
			reportStartTime("BulkLoad/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void bulkLoadEnd() {
			reportEndTime("BulkLoad/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void dumpFilesStart() {
			reportStartTime("DumpFiles--" + currentOntologyBeingProcessed);
		}

		public void dumpFilesEnd() {
			reportEndTime("DumpFiles--" + currentOntologyBeingProcessed);

		}

		public void buildSchemaStart() {
			reportStartTime("BuildSchema--" + currentOntologyBeingProcessed);
		}

		public void buildSchemaEnd() {
			reportEndTime("BuildSchema--" + currentOntologyBeingProcessed);
		}

		public void loadTsvFilesStart() {
			reportStartTime("LoadTsvFiles--" + currentOntologyBeingProcessed);
		}

		public void loadTsvFilesEnd() {
			reportEndTime("LoadTsvFiles--" + currentOntologyBeingProcessed);
		}

		public void updateStart() {
			reportStartTime("Update/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void updateEnd() {
			reportEndTime("Update/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void startOntologyLoad() {
			reportStartTime("Obo To OWL Conversion--"
					+ currentOntologyBeingProcessed);
		}

		// public void endOntologyLoad(OWLGraphWrapper graph) {
		public void endOntologyLoad(Object object) {
			reportEndTime("Obo To OWL Conversion--" + currentOntologyBeingProcessed);

			if (object instanceof OWLOntology) {
				OWLOntology ont = (OWLOntology) object;
				if (ontologyGraph == null) {
					try {
						ontologyGraph = new OWLGraphWrapper(ont);
					} catch (Exception ex) {
						LOG.error(ex.getMessage(), ex);
						this.exception = ex;
					}
				} else {
					ontologyGraph.addSupportOntology(ont);
				}
			}
		}
		
		
	}
	
}
