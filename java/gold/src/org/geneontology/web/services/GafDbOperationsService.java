package org.geneontology.web.services;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.zip.GZIPInputStream;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafHibObjectsBuilder;
import org.geneontology.gaf.hibernate.GafObjectsFactory;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gaf.io.GafURLFetch;
import org.geneontology.gold.hibernate.model.GoldObjectFactory;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.rules.AnnotationRuleCheckException;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.geneontology.gold.rules.AnnotationRulesEngine;
import org.geneontology.solrj.GafSolrLoader;

import owltools.gaf.GafParserListener;

/**
 * This class performs bulkload and update operations for GAF database against
 * the web based requests.
 * 
 * @author Shahid Manzoor
 * 
 */
public class GafDbOperationsService extends ServiceHandlerAbstract{

	private static Logger LOG = Logger.getLogger(GafDbOperationsService.class);

	public static final String SERVICE_NAME = "gaf-db-operations";

	/**
	 * Path of the jsp file which renders the results of the computations of
	 * this service.
	 */
	private String viewPath;

	/**
	 * The GAF files paths
	 */
	private List<GafURLFetch> gafLocations;

	/**
	 * The thread which runs the bulkload and update operations in background.
	 */
	private GafDbTaskExecution runner;

	/**
	 * It holds the value of the 'force' request parameter. The parameter is
	 * used in bulkload. If the value is true then delete gold database tables
	 * and create from scratch.
	 */
	// private boolean force;

	/**
	 * It holds the collections gene annotation voilations.
	 */
	private Set<AnnotationRuleViolation> annotationRuleViolations;

	//storing command line name, e.g update, bulkload, runrules
	private String command;

	//true if the commad is update
	private boolean update;
	//true if command is bulkload
	private boolean bulkload;

	//true if annotation checks are requested
	private boolean runAnnotationRules;

	//true if solr load is requested
	private boolean solrLoad;

	/**
	 * If the service completes its operation then the user 
	 * is not allowed to re-run this service again. User has to
	 * send new request to run another operation.
	 */
	private boolean complete;

	/**
	 * It keeps track if auny of the update operation ,e.g bulkload, update and solrload, is in-progress.
	 * A new update operation is not allowed if a update operation is already in progress.
	 */
	private static boolean updateInProgress;
	
	/**
	 * Each GAF operation runs a thread in background. The web browser calls the
	 * handle service method in intervals to check whether the operation is
	 * completed or not. On the last call all the variables are reset. But there
	 * are situations when the output is other than the html and in those
	 * situation the service for a specific operation will be called once so in
	 * those cases after the end of operation variables reset.
	 */
	// private boolean noReloadMode;

	public GafDbOperationsService() {
		runner = null;
		// isOPerationCompleted = true;
		annotationRuleViolations = new HashSet<AnnotationRuleViolation>();
		annotationRuleViolations = Collections
				.synchronizedSet(annotationRuleViolations);
	}

	/**
	 * This service performs the computations and stores the computations
	 * results via request.setAttribute method. This request object attributes
	 * values are visible in the jsp file associated with this service. The jsp
	 * file prints the values during its rendering process. 
	 * 
	 * This method collects the parameter from the request object. And initialize
	 * the thread object which performs the requested operation,i.e. bulkload, update, solrload and run annotations check 
	 */
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		try {

			command = request.getParameter("command");
			update = "update".equals(request.getParameter("command"));
			bulkload = "bulkload".equals(request.getParameter("command"));
			runAnnotationRules = "runrules".equals(command) ? true : false;
			solrLoad = "solrload".equals(command) ? true : false;
			String remoteGAF = request.getParameter("remote-gaf");
			String fileLocation = request.getParameter("filelocation");

			String view = request.getParameter("view");
			// set the default view
			viewPath = "/servicesui/gafdb.jsp";

			if (view != null) {
				viewPath = "/servicesui/" + view + ".jsp";

			}

			if ("getlastupdate".equals(command)) {

					viewPath = "/servicesui/gold-lastupdate.jsp";
					
					Hashtable<String, String> dbs = new Hashtable<String, String>();
					request.setAttribute("dbs", dbs);
					GoldObjectFactory f = GoldObjectFactory.buildDefaultFactory();
					GafObjectsFactory factory = new GafObjectsFactory();
					for(GafDocument doc: factory.getGafDocument()){
						dbs.put(doc.getId(), f.getLatestDatabaseChangeStatus(doc.getId()).getChangeTime().toString());
					}
				
			}
			/**
			 * The below condition sets gaf documents location
			 */
			else if (runner == null && !complete) {
				
				gafLocations = new ArrayList<GafURLFetch>();
				if (remoteGAF != null) {
					//gafLocations = new GafURLFetch(remoteGAF);
					gafLocations.add(new GafURLFetch(remoteGAF));
				}else {

					List gafFiles = null;
					
					if (fileLocation != null){
						gafFiles = new ArrayList();
						gafFiles.add(fileLocation);
					}else{
						gafFiles = GoConfigManager.getInstance()
						.getDefaultGafFileLocations();
						
					}
					
					List<String> files = (List<String>) gafFiles;
					//List<GafURLFetch> gafURLs = new ArrayList<GafURLFetch>();
					for (String ontLocation : files) {

						if (ontLocation.startsWith("http:/") || ontLocation.startsWith("ftp:/")){
							gafLocations.add( new GafURLFetch(ontLocation));
						}else{
							File file = null;
							
							if(ontLocation.startsWith("file:/")){
								try{
									file = new File(new URI(ontLocation));
								}catch(Exception ex){
									LOG.error(ex.getMessage(), ex);
									throw new IOException(ex);
								}
							}else{
								file = new File(ontLocation);
								File  files2[] =new File[]{ file};
							
							
								if(file.isDirectory()){
									files2 = file.listFiles();
								}
								
								
								for(File f: files2){
									if( f.isDirectory() || f.getName().endsWith(".")){
										continue;
									}
									
									gafLocations.add(new GafURLFetch(f.toURI().toString()));
								}
							}
							
						}
					}
					
					
					
					
					
					

				}
				
				if(!((update || bulkload )&& updateInProgress)){
					runner = new GafDbTaskExecution();
					runner.start();
				}
				
				if(update || bulkload || solrLoad)
					updateInProgress = true;

			}

		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
			throw new IOException(ex);
		}
		
		finally {

			// store information in the request object. The request object is
			// available in the
			// jsp file. The jsp use the objects data in print html.
			//if (runAnnotationRules)
			request.setAttribute("violations", annotationRuleViolations);

			if (runner != null) {
				request.setAttribute("task", runner);
				request.setAttribute("exception", runner.getException());
			}
			request.setAttribute("isTaskRunning", runner == null ? false
					: runner.isRunning());
			request.setAttribute("command", command);
			// request.setAttribute("isLargeFile", isLargefile);

			// if the task has completed its operation then set it to null
		/*	if (runner != null && !runner.isRunning()) {
				runner = null;
			}*/
			
		}

	}

	public static boolean isDbUpdateInProgress(){
		return updateInProgress;
	}
	
	public String getServiceName() {
		return SERVICE_NAME;
	}

	public String getViewPath() {
		return this.viewPath;
	}

	/**
	 * The execute method is called by the {@link Task} class. This class
	 * executes the update and bulkload methods of {@link GAFDbOperations}. The
	 * implemented listener methods of the {@link DbOperationsListener}
	 * interface are called {@link GAFDbOperations}. The listener methods keep
	 * stores start and end time of sub task of the the bulkload and update
	 * operation. The subtasks completion time is printed by the jsp associated
	 * with the GafDbOperationsService class.
	 * 
	 * @author Shahid Manzoor
	 * 
	 */
	// class GafDbTaskExecution extends Task implements TaskExecution,
	// DbOperationsListener{
	class GafDbTaskExecution extends Task implements DbOperationsListener, GafParserListener {

		// id of the current
		private String currentOntologyBeingProcessed;

		// private Exception exception;
		GAFDbOperations db = null;

		public GafDbTaskExecution() {
			this.data = this;
			db = new GAFDbOperations();
			//db.addDbOperationsListener(this);
		}

		@Override
		public void execute() {


		//	try {


				for (GafURLFetch fetch: gafLocations) {
					//GafURLFetch fetch = (GafURLFetch) gafLocations;
					try{
							fetch.connect();
							while (fetch.hasNext()) {
		
								
									InputStream is = (InputStream) fetch.next();
									java.io.Reader reader = new InputStreamReader(is);
									buildGafDocument(reader,
											fetch.getCurrentGafFile(),
											fetch.getCurrentGafFilePath());
										
										//gafDocuments.add(doc);
									reader.close();
									is.close();
									fetch.completeDownload();
							}
						}catch (Exception ex) {
							this.exceptions.add(new Exception("The operation is failed for the file " + fetch.getCurrentGafFile() , ex));
						//	this.exception = ex;
							LOG.error(ex, ex);
						}finally{
							//gafDocuments = null;
							//splittedDocuments = null;
							complete = true;
							updateInProgress = false;
						}
						
						
						
					}
	/*			}

			} catch (Throwable ex) {
				//this.exception = ex;
				this.exceptions.add(new Exception("Operation is failed for the file " + fetch., arg1))
				LOG.error(ex, ex);

			}finally{
				//gafDocuments = null;
				//splittedDocuments = null;
				complete = true;
				updateInProgress = false;
			}*/
		}

		
		/**
		 * Run annotation checks rule here
		 * @param doc
		 * @throws AnnotationRuleCheckException
		 */
		private void _performAnnotationChecks(GafDocument doc) throws AnnotationRuleCheckException {
			LOG.info("Performing Annotation Checks");
			AnnotationRulesEngine engine = AnnotationRulesEngine.getInstance();

			Set<AnnotationRuleViolation> violations = engine
					.validateAnnotations(doc);

			annotationRuleViolations.addAll(violations);
			
			LOG.info("Annotation Checks are completed.");

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

		public void startDomLoad() {
			reportStartTime("Parsing GAF--" + currentOntologyBeingProcessed);
		}

		public void endDomLoad(Object object) {
			reportEndTime("Parsing GAF--" + currentOntologyBeingProcessed);

			/*if (object instanceof GafHibObjectsBuilder) {
				GafHibObjectsBuilder builder = (GafHibObjectsBuilder) object;
				// gafDocuments.add(builder.getGafDocument());

				for(Object v: builder.getParser().getAnnotationRuleViolations())
					annotationRuleViolations.add(new AnnotationRuleViolation(v+""));

			}*/

		}

		/*
		 * private GafDocument buildGafDocument(Reader reader, String docId,
		 * String path) throws IOException{ this.currentOntologyBeingProcessed =
		 * path; startOntologyLoad();
		 * 
		 * GafObjectsBuilder builder = new GafObjectsBuilder();
		 * 
		 * GafDocument doc = builder.buildDocument(reader, docId, path);
		 * 
		 * GafDocument d = null;
		 * 
		 * while((d = builder.getNextSplitDocument() ) != null){
		 * LOG.info("Splitting the '" + path + "' document");
		 * splittedDocuments.add(path); doc = null; }
		 * 
		 * endOntologyLoad(builder);
		 * 
		 * return buildGafDocument(reader, docId, path, false); }
		 */

		/*private void buildSplittedDocuments() throws Exception {
			for (String path : splittedDocuments) {
				if (path.startsWith("http://") || path.startsWith("ftp:/")) {
					GafURLFetch fetch = new GafURLFetch(path);
					fetch.connect();
					InputStream is = (InputStream) fetch.next();

					InputStreamReader reader = new InputStreamReader(is);
					buildGafDocument(reader, fetch.getCurrentGafFile(), path);
					is.close();
					reader.close();
					fetch.completeDownload();
				} else {
					File f = new File(path);
					InputStream is = new FileInputStream(f);

					if (f.getName().endsWith(".gz")) {
						is = new GZIPInputStream(is);
					}
					buildGafDocument(new InputStreamReader(is), f.getName(),
							path);

					is.close();
				}
			}
		}*/

		private void buildGafDocument(Reader reader, String docId,
				String path) throws Exception {
			this.currentOntologyBeingProcessed = docId;

			if(bulkload){
				reportStartTime(path+"bulkload");
				db.bulkload(reader, docId, path, false);
				reportEndTime(path+"bulkload");
				return;
			}else if(update){
				reportStartTime(path+"update");
				db.update(reader, docId, path);
				reportEndTime(path+"update");
				return;
				
			}

			startDomLoad();

			GafHibObjectsBuilder builder = new GafHibObjectsBuilder();
			
			builder.getParser().addParserListener(this);
			
			GafDocument doc = builder.buildDocument(reader, docId, path);

			GafDocument d = doc;

			/*GAFDbOperations db = null;
			if (update) {
				db = new GAFDbOperations();
				db.addDbOperationsListener(this);
			}*/

			int splitSize = GoConfigManager.getInstance().getSplitSize() * 4;
			do {
				if(d == null)
					break;
				

				if (annotationRuleViolations.size() >= splitSize) {
					throw new Exception(
							"The annotations check is terminated as the annotations violations messages are not being consumed.");
				}

				LOG.info("Splitting the '" + path + "' document");

				/*if (doc != null && !splittedDocuments.contains(path)) {
					splittedDocuments.add(path);
					// isLargefile = true;
				}*/

				/*if (update) {
				//	db.update(d);
					db.update(d, true);
				}else if (bulkload){
					db.bulkLoad(d, false);
				}*/

				if (solrLoad) {
					GafSolrLoader loader = new GafSolrLoader(
							GoConfigManager.getInstance().getSolrUrl());

					reportStartTime("Loading into Solr--"
							+ currentOntologyBeingProcessed);
					
					loader.load(d);
					
					reportEndTime("Loading into Solr--"
							+ currentOntologyBeingProcessed);
					
				}

				else if (runAnnotationRules) {
					
					reportStartTime("Performing Annotations Checks--"
							+ currentOntologyBeingProcessed);
					_performAnnotationChecks(d);
					reportEndTime("Performing Annotations Checks--"
							+ currentOntologyBeingProcessed);
					
					
				}

			}while ((d = builder.getNextSplitDocument()) != null);

			
			/*if(update){
				db.update(null, true);
			}*/
			
			endDomLoad(builder);

		}

		@Override
		public void parserError(String message, String row, int lineNumber) {
			AnnotationRuleViolation av = new AnnotationRuleViolation(message);
			av.setAnnotationRow(row);
			av.setLineNumber(lineNumber);
			av.setGafDoument(this.currentOntologyBeingProcessed);
			av.setRuleId("Parsing Error");
			
			annotationRuleViolations.add(av);
		}

		@Override
		public void parsing(String arg0, int arg1) {
			// TODO Auto-generated method stub
			
		}

	}


}
