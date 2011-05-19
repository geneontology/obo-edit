package org.geneontology.web.services;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
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
	
	/**
	 * Each GAF operation runs a thread in background. The web browser calls the handle service method
	 * in intervals to check whether the operation is completed or not. On the last call all the variables 
	 * are reset. But there are situations when the output is other than the html and in those situation the service
	 * for a specific operation will be called once so in those cases after the end of operation variables reset.
	 */
	private boolean noReloadMode;
	
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
	 * 
	 * This service performs some of the tasks in multiple steps. The first step for the task except the 'getlastupdate'
	 * is to load gaf documents. It tries to load three locations: 1)system default location, 2) remote location (http/ftp urls)
	 * and (3) through the filelocation parameter.
	 */
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		try{
		
			this.noReloadMode = false;
		//	this.force = "true".equals(request.getParameter("force"));
			command = request.getParameter("command");
			commit = request.getParameter("commit") == null ? false : true;
			runAnnotationRules = request.getParameter("runrules") == null ? false : true;
			solrLoad = request.getParameter("solrload") == null ? false : true;
			String format = request.getParameter("format");
			String remoteGAF = request.getParameter("remote-gaf");
			String fileLocation = request
			.getParameter("filelocation");
			
			//set the default view
			viewPath = "/servicesui/gafdb.jsp";
			
			
			if(runner == null){
			
				if("json".equals(format)){
					viewPath = "/servicesui/gafjson.jsp";
					this.noReloadMode = true;
				}
				
				if("getlastupdate".equals(command)){
					viewPath = "/servicesui/gold-lastupdate.jsp";
				}else {
				
					
//					if(!commit && !solrLoad && this.gafDocuments == null){				
					if((!commit && !solrLoad && !runAnnotationRules) || noReloadMode){				

					////////////////////start guessing gaf file location
						
						//if there is no task running then create one for the update and bulkload commands
			//			if(isOPerationCompleted && !commit) {
						this.gafDocuments = null;
						if(remoteGAF != null){
								gafLocations = new GafURLFetch(remoteGAF);
						}else{
			
					
							if(fileLocation != null){
								gafLocations = new ArrayList<String>();
								((ArrayList)gafLocations).add(fileLocation);
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
									command = "bulkload";
								}
							}
						}
			
						//////////start guessing gaf file location
					}else if(gafDocuments == null){//if commit is calld prior to the bulkload nand upate then throw error
						request.setAttribute("exception", new IllegalStateException("The commit is not allowed."));
					}
					
				 //if(("bulkload".equals(command) || "update".equals(command) || runAnnotationRules) && (this.gafLocations != null || this.gafDocuments != null)) {
					annotationRuleViolations.clear();
					runner = new GafDbTaskExecution();
					runner.start();
						
					
				}
				
			} 
			
			
			//store information in the request object. The request object is available in the 
			//jsp file. The jsp use the objects data in print html.
			if(runAnnotationRules || !commit || !solrLoad)
				request.setAttribute("violations", annotationRuleViolations);
			
			if(runner != null){
				request.setAttribute("task", runner.getData());
			}
			
			
		}finally{
			request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
			request.setAttribute("command", command);
			
			//if the task has completed its operation then set it to null
			if(runner != null && !runner.isRunning()){
				runner = null;
			}
			
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
				if(solrLoad) {

					GafSolrLoader loader = new GafSolrLoader(GeneOntologyManager.getInstance().getSolrUrl());

					for(GafDocument gafDocument: gafDocuments){
						currentOntologyBeingProcessed = gafDocument.getDocumentPath();
						reportStartTime("Loading into Solr--" + currentOntologyBeingProcessed);

						loader.load(gafDocument);
						
						reportEndTime("Loading into Solr--" + currentOntologyBeingProcessed);

					}
					
					gafDocuments = null;
					
				}else				
				if(commit){
					for(GafDocument gafDocument: gafDocuments){
						this.currentOntologyBeingProcessed = gafDocument.getId();
					
						if("bulkload".equals(command)){
							db.update(gafDocument);
						}else if("update".equals(command)){
							db.update(gafDocument);
						}
					}
					gafDocuments = null;
				}else{
					
					if(gafDocuments == null){
						if(gafLocations instanceof GafURLFetch){
							GafURLFetch fetch = (GafURLFetch) gafLocations;
							fetch.connect();
							gafDocuments = new ArrayList<GafDocument>();
							while(fetch.hasNext()){
	
								InputStream is = (InputStream)fetch.next();
								this.currentOntologyBeingProcessed = fetch.getCurrentGafFile();
								java.io.Reader reader = new InputStreamReader(is);
								GafDocument doc = db.buildGafDocument(reader, fetch.getCurrentGafFile(), fetch.getCurrentGafFilePath());
								gafDocuments.add( doc);
								reader.close();
								is.close();
								fetch.completeDownload();
							//	performAnnotationChecks(doc);
							}
						}else{
							
							List<String> files = (List<String>)gafLocations;
							gafDocuments = new ArrayList<GafDocument>();
							for(String ontLocation: files){
								
								File file = null;
								
								if(ontLocation.startsWith("http:/") || ontLocation.startsWith("file:/")){
									file = new File(new URI(ontLocation));
								}else
									file = new File(ontLocation);
								
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
										if("bulkload".equals(command) || "update".equals(command) || runAnnotationRules){
											GafDocument doc = db.buildGafDocument(ontLocation);
											gafDocuments.add( doc );
									//		performAnnotationChecks(doc);
	
										}
									}
							}
		
						}
					
					}
					
					
					if(runAnnotationRules){
						for(GafDocument gafDocument: gafDocuments){
							currentOntologyBeingProcessed = gafDocument.getDocumentPath();
							performAnnotationChecks(gafDocument);
						}
						
					}						
					
					
				}
					
					
					
			}catch(Throwable ex){
				gafDocuments = null;
				this.exception = ex;
				LOG.error(ex, ex);
				
			}
			
			if(noReloadMode){
				runner = null;
				gafDocuments = null;
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

				for (AnnotationRuleViolation v : builder.getParser().getAnnotationRuleViolations()) {
					annotationRuleViolations
							.add(v);
				}

			}
			
			
		}
		
		
	}

}
