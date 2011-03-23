package org.geneontology.web.services;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
import org.geneontology.gaf.io.GafURLFetch;
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
	private Object gafLocations;
	
	/**
	 * The thread which runs the bulkload and update operations
	 * in background.
	 */
	private GafDbTaskExecution runner;
	
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
	
	private boolean commit;
	
//	private boolean isOPerationCompleted;
	
	private List<GafDocument> gafDocuments;
	
	
	public GafDbOperationsService(){
		runner = null;
	//	isOPerationCompleted = true;
		annotationRuleViolations = new HashSet<AnnotationRuleViolation>();
	}
	
	/**
	 * This service performs the computations and stores the computations results via request.setAttribute method.
	 * This request object attributes values are visible in the jsp file associated with this service. 
	 * The jsp file prints the values during its rendering process.   
	 * update and bulkload is a two stage process. In the first stage GAF file parsed and annotation rules are run. A summary
	 * of rules voilation is represented to the user. The second stage is commit which makes changes in the database.
	 */
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

	
		
		try{
		
		//	this.force = "true".equals(request.getParameter("force"));
			command = request.getParameter("command");
			commit = request.getParameter("commit") == null ? false : true;
			//set the default view
			viewPath = "/servicesui/gafdb.jsp";
			
			String remoteGAF = request.getParameter("remote-gaf");
			
			//if there is no task running then create one for the update and bulkload commands
//			if(isOPerationCompleted && !commit) {
			if(remoteGAF != null){
					gafLocations = new GafURLFetch(remoteGAF);
			}else if(runner == null && !commit) {
			
				//update command expects gaf location location in the parameter.
				//If no gaflocatoin parameter is set then present 
				//a form to user to select gaf file
				if ("update".equals(command)) {
		
					String ontologylocation = request
							.getParameter("ontologylocation");
					
					if(ontologylocation != null){
						gafLocations = new ArrayList<String>();
						((ArrayList)gafLocations).add(ontologylocation);
						gafDocuments = null;
					}else{
						request.setAttribute("servicename", getServiceName());
						request.setAttribute("locations", GeneOntologyManager.getInstance().getDefaultGafFileLocations());
						
						this.viewPath = "/servicesui/golddb-updateform.jsp";
					}
				}else if("bulkload".equals(command)){
					this.gafLocations = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
					gafDocuments = null;
				}else if("getlastupdate".equals(command)){
					viewPath = "/servicesui/gold-lastupdate.jsp";
				}
			}
			
			//if commit is calld prior to the bulkload nand upate then throw error
			if(commit && gafDocuments == null){
				request.setAttribute("exception", new IllegalStateException("The commit is not allowed."));
			}else if((commit || "bulkload".equals(command) || "update".equals(command)) && runner == null) {
				annotationRuleViolations.clear();
				runner = new GafDbTaskExecution();
				runner.start();
				
			}
			
			//store information in the request object. The request object is available in the 
			//jsp file. The jsp use the objects data in print html.
			if(!commit)
				request.setAttribute("violations", annotationRuleViolations);
			
//			request.setAttribute("dbname", "GAF");
			
			if(runner != null){
				request.setAttribute("task", runner.getData());
			}
		}finally{
			
			//if the task has completed its operation then set it to null
			if(runner != null && !runner.isRunning()){
				runner = null;
				//				isOPerationCompleted = true;
			}
			
			request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
			
		}
		
		
		
		
	}
	
	public String getServiceName() {
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
		
			
			
			
			/*for(TaskExecutionListener l: listeners){
				l.updateData(this);
			}*/
			
			GAFDbOperations db = new GAFDbOperations();
			db.addDbOperationsListener(this);
			
			try{
				if(!commit){
					
					if(gafLocations instanceof GafURLFetch){
						GafURLFetch fetch = (GafURLFetch) gafLocations;
						fetch.connect();
						gafDocuments = new ArrayList<GafDocument>();
						while(fetch.hasNext()){
							InputStream is = (InputStream)fetch.next();
							gafDocuments.add(db.buildGafDocument(new InputStreamReader(is)));
						}
					}else{
						List<String> files = (List<String>)gafLocations;
						for(String ontLocation: files){
							this.currentOntologyBeingProcessed = ontLocation;
						
								gafDocuments = new ArrayList<GafDocument>();
								if("bulkload".equals(command) || "update".equals(command)){
									gafDocuments.add( db.buildGafDocument(ontLocation) );
								}
							}
	
						GoldDbOperationsService goldDb = (GoldDbOperationsService) ServicesConfig.getService("gold-db-operations");
						
						OWLGraphWrapper graph = goldDb.getGraphWrapper();
						
						for (GafDocument doc : gafDocuments) {
	
							annotationRuleViolations.addAll(doc
									.validateAnnotations(graph));
						}
					}
					
				//	wait();
				}else{
						
						for(GafDocument gafDocument: gafDocuments){
						
							if("bulkload".equals(command)){
								db.bulkLoad(gafDocument, false);
							}else if("update".equals(command)){
								db.update(gafDocument);
							}
						}
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
