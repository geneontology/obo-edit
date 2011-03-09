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

import owltools.graph.OWLGraphWrapper;

/**
 * This class performs bulkload and update operations for GAF database against the
 * web based requests.
 *  
 * @author Shahid Manzoor
 *
 */
public class GafDbOperationsService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(GafDbOperationsService.class);
	
	/**
	 * Path of the jsp file which renders the results of the computations of this service.
	 */
	private String viewPath;
	
	/**
	 * The GAF files paths
	 */
	private List<String> gafLocations;
	
	/**
	 * The thread which runs the bulkload and update operations
	 * in background.
	 */
	private Task runner;
	
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
		runner = null;
	}
	
	/**
	 * This service performs the computations and stores the computations results via request.setAttribute method.
	 * This request object attributes values are visible in the jsp file associated with this service. 
	 * The jsp file prints the values during its rendering process.   
	 */
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		this.force = "true".equals(request.getParameter("force"));
		command = request.getParameter("command");
	
		//set the default view
		viewPath = "/servicesui/golddb.jsp";
		//if there is no task running then create one for the update and bulkload commands
		if(runner == null){
		
			//update command expects gaf location location in the parameter.
			//If no gaflocatoin parameter is set then present 
			//a form to user to select gaf file
			if ("update".equals(command)) {
	
				String ontologylocation = request
						.getParameter("ontologylocation");
				
				
				if(ontologylocation != null){
					gafLocations = new ArrayList<String>();
					gafLocations.add(ontologylocation);
					runner = new GafDbTaskExecution();
				}else{
					request.setAttribute("servicename", getServiceName());
					request.setAttribute("locations", GeneOntologyManager.getInstance().getDefaultGafFileLocations());
					
					this.viewPath = "/servicesui/golddb-updateform.jsp";
				}
			}else if("bulkload".equals(command)){
				this.gafLocations = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
				runner = new GafDbTaskExecution();
			}else if("getlastupdate".equals(command)){
				viewPath = "/servicesui/gold-lastupdate.jsp";
			}

			//run the task in backgorund
			if(this.runner != null){
				runner.start();
			}
			
			
		}
		
		//store information in the request object. The request object is available in the 
		//jsp file. The jsp use the objects data in print html.
		request.setAttribute("violations", annotationRuleViolations);
		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		request.setAttribute("dbname", "GAF");
		
		if(runner != null){
			request.setAttribute("task", runner.getData());
		}
		
		//if the task has completed its operation then set it to null
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
	
	
	/**
	 * The execute method is called by the {@link Task} class. This class executes the
	 * update and bulkload methods of {@link GAFDbOperations}. The implemented listener  methods of the
	 * {@link DbOperationsListener} interface are called {@link GAFDbOperations}. The listener methods keep
	 * stores start and end time of sub task of the the bulkload and update operation. The subtasks completion
	 * time is printed by the jsp associated with the GafDbOperationsService class. 
	 * 
	 * @author Shahid Manzoor
	 *
	 */
//	class GafDbTaskExecution extends Task implements TaskExecution, DbOperationsListener{
	class GafDbTaskExecution extends Task implements DbOperationsListener{

		// id of the current
		private String currentOntologyBeingProcessed;
		
	//	private Exception exception;
		
		public GafDbTaskExecution(){
			this.data = this;
		}
		
		
		@Override
		public void execute() {
		
			List<GafDocument> gafDocuments = new ArrayList<GafDocument>();
			
			annotationRuleViolations = new HashSet<AnnotationRuleViolation>();
			
			/*for(TaskExecutionListener l: listeners){
				l.updateData(this);
			}*/
			
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

/*		@Override
		public Object getData() {
			return this;
		}

		@Override
		public void addTaskExecutionListener(TaskExecutionListener listener) {
			listeners.add(listener);
		}
*/

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
