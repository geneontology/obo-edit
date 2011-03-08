package org.geneontology.web.services;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.geneontology.web.Task;
import org.geneontology.web.TaskExecution;
import org.geneontology.web.TaskExecutionListener;
import org.geneontology.web.TaskRunner;

import owltools.graph.OWLGraphWrapper;

/**
 * This class performs bulkload and update operations against the
 * web based requests.
 *  
 * @author Shahid Manzoor
 *
 */
public class GafDbOperationsService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(GafDbOperationsService.class);
	
//	private List<GafDocument> gafDocuments;

	/**
	 * Path of the jsp file which renders the results of the computations of this service.
	 */
	private String viewPath;
	
	/**
	 * The GAF file paths
	 */
	private List<String> gafLocations;
	
	/**
	 * The thread which runs the bulkload and update operations
	 * in background.
	 */
	private TaskRunner runner;
	
	/**
	 * It holds the value of the 'force' request parameter. The parameter is used in bulkload.
	 * If the value is true then delete gold database tables and create from scratch.
	 */
	private boolean force;
	
	/**
	 * It holds the collections gene annotation voilations.
	 */
	private Set<AnnotationRuleViolation> annotationRuleViolations;	
	
	private String command;
	
	public GafDbOperationsService(){
		
		
		this.gafLocations = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
		runner = null;
	}
	
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		this.force = false;
		command = request.getParameter("command");
	
		viewPath = "/servicesui/golddb.jsp";
		
		if(runner == null){
		
			if ("update".equals(command)) {
	
				String ontologylocation = request
						.getParameter("ontologylocation");
				
				
				if(ontologylocation != null){
					gafLocations = new ArrayList<String>();
					gafLocations.add(ontologylocation);
					runner = new TaskRunner(new GafDbTaskExecution());
				}else{
					request.setAttribute("servicename", getServiceName());
					request.setAttribute("locations", GeneOntologyManager.getInstance().getDefaultGafFileLocations());
					
					this.viewPath = "/servicesui/golddb-updateform.jsp";
				}
			}else if("bulkload".equals(command)){
				this.force = "true".equals(request.getParameter("force"));
				runner = new TaskRunner(new GafDbTaskExecution());
			}else if("getlastupdate".equals(command)){

				viewPath = "/servicesui/gold-lastupdate.jsp";
				
				
			}

			if(this.runner != null){
				runner.start();
			}
			
			
		}
		request.setAttribute("violations", annotationRuleViolations);
		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		request.setAttribute("dbname", "GAF");
		
		if(runner != null){
			request.setAttribute("task", runner.getData());
		}
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}
		
		
	}
	
	@Override
	public String getServiceName() {
		// TODO Auto-generated method stub
		return "gaf-db-operations";
	}

	public String getViewPath(){
		return this.viewPath;
	}
	
	
	
	class GafDbTaskExecution extends Task implements TaskExecution, DbOperationsListener{

		private List<TaskExecutionListener> listeners;

		// id of the current
		private String currentOntologyBeingProcessed;
		
	//	private Exception exception;
		
		public GafDbTaskExecution(){
			listeners = new ArrayList<TaskExecutionListener>();
		}
		
		
		@Override
		public void execute() {
		
			List<GafDocument> gafDocuments = new ArrayList<GafDocument>();
			
			annotationRuleViolations = new HashSet<AnnotationRuleViolation>();
			
			for(TaskExecutionListener l: listeners){
				l.updateData(this);
			}
			
			GAFDbOperations db = new GAFDbOperations();
			db.addDbOperationsListener(this);
			
			try{
					for(String ontLocation: gafLocations){
						this.currentOntologyBeingProcessed = ontLocation;
						
						if("bulkload".equals(command))
							db.bulkLoad(ontLocation, force);
						else 
							db.update(ontLocation);
					}
					
					GoldDbOperationsService goldDb = (GoldDbOperationsService) ServicesConfig.getService("gold-db-operations");
					
					OWLGraphWrapper graph = goldDb.getGraphWrapper();
					
					for (GafDocument doc : gafDocuments) {

						annotationRuleViolations.addAll(doc
								.validateAnnotations(graph));
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

		public void endOntologyLoad(Object object) {
			reportEndTime("Obo To OWL Conversion--" + currentOntologyBeingProcessed);

			if (object instanceof GafObjectsBuilder) {
				GafObjectsBuilder builder = (GafObjectsBuilder) object;
		//		gafDocuments.add(builder.getGafDocument());

				annotationRuleViolations.addAll(builder.getParser()
						.getAnnotationRuleViolations());

				for (String s : builder.getParser().getErrors()) {
					annotationRuleViolations
							.add(new AnnotationRuleViolation(s));
				}

			}
			
			
		}
		
		
	}

}
