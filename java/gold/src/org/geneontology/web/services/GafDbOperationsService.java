package org.geneontology.web.services;

import java.io.File;
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
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gaf.io.GafURLFetch;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.geneontology.gold.rules.AnnotationRulesEngine;
import org.geneontology.solrj.GafSolrLoader;
import org.geneontology.web.Task;

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
	
	private boolean runAnnotationRules;
	
	private boolean solrLoad;

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
			runAnnotationRules = request.getParameter("runrules") == null ? false : true;
			solrLoad = request.getParameter("solrload") == null ? false : true;
			
			//set the default view
			viewPath = "/servicesui/gafdb.jsp";
			
			
			if("getlastupdate".equals(command)){
				viewPath = "/servicesui/gold-lastupdate.jsp";
			}else {
			
				
				if(!commit && !runAnnotationRules && !solrLoad && runner == null){				
					////////////////////start guessing gaf file location
					String remoteGAF = request.getParameter("remote-gaf");
					
					//if there is no task running then create one for the update and bulkload commands
		//			if(isOPerationCompleted && !commit) {
					if(remoteGAF != null){
							gafLocations = new GafURLFetch(remoteGAF);
					}else{
		
						String fileLocation = request
						.getParameter("filelocation");
				
						if(fileLocation != null){
							gafLocations = new ArrayList<String>();
							((ArrayList)gafLocations).add(fileLocation);
							gafDocuments = null;
						}else{
						
							GafObjectsFactory factory = new GafObjectsFactory();
							
							List list = factory.getGafDocument();
							
							//update command expects gaf location location in the parameter.
							//If no gaflocatoin parameter is set then present 
							//a form to user to select gaf file
							if ("update".equals(command) && !list.isEmpty()) {
					
								if(fileLocation==null){
									request.setAttribute("servicename", getServiceName());
									request.setAttribute("locations", GeneOntologyManager.getInstance().getDefaultGafFileLocations());
									
									this.viewPath = "/servicesui/golddb-updateform.jsp";
								}
							}else if("bulkload".equals(command) || (list.isEmpty() && "update".equals(command)) ){
								
								if(fileLocation == null){
									this.gafLocations = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
								}
								gafDocuments = null;
								command = "bulkload";
							}
						}
					}
		
					//////////start guessing gaf file location
				}
	
				
				//if commit is calld prior to the bulkload nand upate then throw error
				if((commit || runAnnotationRules || solrLoad) && gafDocuments == null){
					request.setAttribute("exception", new IllegalStateException("The commit is not allowed."));
				}else if(("bulkload".equals(command) || "update".equals(command)) && runner == null && (this.gafLocations != null || this.gafDocuments != null)) {
					annotationRuleViolations.clear();
					runner = new GafDbTaskExecution();
					runner.start();
					
				}
				
				//store information in the request object. The request object is available in the 
				//jsp file. The jsp use the objects data in print html.
				if(runAnnotationRules || !commit || !solrLoad)
					request.setAttribute("violations", annotationRuleViolations);
				
	//			request.setAttribute("dbname", "GAF");
				
				if(runner != null){
					request.setAttribute("task", runner.getData());
				}
				
				
			} //end of else
			
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
				if(commit){
					for(GafDocument gafDocument: gafDocuments){
						this.currentOntologyBeingProcessed = gafDocument.getId();
					
						if("bulkload".equals(command)){
							db.update(gafDocument);
						}else if("update".equals(command)){
							db.update(gafDocument);
						}
					}				
				}else if(runAnnotationRules){
					Object obj = gafDocuments;
					for(GafDocument gafDocument: gafDocuments){
						currentOntologyBeingProcessed = gafDocument.getDocumentPath();
						performAnnotationChecks(gafDocument);
					}
					
				}else if(solrLoad) {

					GafSolrLoader loader = new GafSolrLoader(GeneOntologyManager.getInstance().getSolrUrl());

					for(GafDocument gafDocument: gafDocuments){
						currentOntologyBeingProcessed = gafDocument.getDocumentPath();
						reportStartTime("Loading into Solr--" + currentOntologyBeingProcessed);

						loader.load(gafDocument);
						
						reportEndTime("Loading into Solr--" + currentOntologyBeingProcessed);

					}
					
				}else{
					
					if(gafLocations instanceof GafURLFetch){
						GafURLFetch fetch = (GafURLFetch) gafLocations;
						fetch.connect();
						gafDocuments = new ArrayList<GafDocument>();
						while(fetch.hasNext()){

							InputStream is = (InputStream)fetch.next();
							this.currentOntologyBeingProcessed = fetch.getCurrentGafFile();
							
							GafDocument doc = db.buildGafDocument(new InputStreamReader(is), fetch.getCurrentGafFile(), fetch.getCurrentGafFilePath());
							gafDocuments.add( doc);
						//	performAnnotationChecks(doc);
						}
					}else{
						List<String> files = (List<String>)gafLocations;
						gafDocuments = new ArrayList<GafDocument>();
						for(String ontLocation: files){
							
							File file = new File(ontLocation);
							
							File dirFiles[] = null;
							
							if(file.isDirectory()){
								dirFiles = file.listFiles();
							}else{
								dirFiles = new File[]{file};
							}
							
							
							for(File f: dirFiles){
								if(!f.isFile() || f.getName().startsWith("."))
									continue;
								ontLocation = f.getAbsolutePath();
								this.currentOntologyBeingProcessed = ontLocation;
									if("bulkload".equals(command) || "update".equals(command)){
										GafDocument doc = db.buildGafDocument(ontLocation);
										gafDocuments.add( doc );
								//		performAnnotationChecks(doc);

									}
								}
						}
	
						
						
					}
					
				}
					
					
					
			}catch(Throwable ex){
				this.exception = ex;
				LOG.error(ex, ex);
			}			
		}


		
		private void performAnnotationChecks(GafDocument doc){
			reportStartTime("Performing Annotations Checks--" + currentOntologyBeingProcessed);
		
			
			AnnotationRulesEngine engine = AnnotationRulesEngine.getInstance();
			
			Set<AnnotationRuleViolation> violations= engine.validateAnnotations(doc);
			
			annotationRuleViolations.addAll(violations);

			reportEndTime("Performing Annotations Checks--" + currentOntologyBeingProcessed);
			
			
		}
		
		protected void reportStartTime(String name) {
			this.addInProgress(name);
		}

		protected void reportEndTime(String name) {
			this.addCompleted(name);
		}

		public void bulkLoadStart() {
			reportStartTime("Load/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void bulkLoadEnd() {
			reportEndTime("Load/TotalTime--" + currentOntologyBeingProcessed);
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
			reportStartTime("Load/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void updateEnd() {
			reportEndTime("Load/TotalTime--" + currentOntologyBeingProcessed);
		}

		public void startOntologyLoad() {
			reportStartTime("Parsing GAF--"
					+ currentOntologyBeingProcessed);
		}

		public void endOntologyLoad(Object object) {
			reportEndTime("Parsing GAF--" + currentOntologyBeingProcessed);

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
