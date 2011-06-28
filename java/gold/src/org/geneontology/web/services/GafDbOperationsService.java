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
import java.util.List;
import java.util.Set;
import java.util.zip.GZIPInputStream;
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
import org.geneontology.gold.rules.AnnotationRulesEngine;
import org.geneontology.solrj.GafSolrLoader;
import org.geneontology.web.Task;

/**
 * This class performs bulkload and update operations for GAF database against
 * the web based requests.
 * 
 * @author Shahid Manzoor
 * 
 */
public class GafDbOperationsService extends ServiceHandlerAbstract {

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
	private Object gafLocations;

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

	private String command;

	private boolean update;

	private boolean runAnnotationRules;

	private boolean solrLoad;

//	private List<GafDocument> gafDocuments;

//	private List<String> splittedDocuments;

	private boolean complete;

	private static boolean updateInProgress;
	
	// private boolean annotationRulesComplete;

	// When annotation checks completed this variable set to true
	// This variable helps to prevent re-run the annotation checks
	// private boolean annotationChecksCompleted;

	// When commit operation is completed this variable is set to true
	// This prevent to execute commit multiple times
	// private boolean commitCompleted;

	// / private boolean isLargefile;

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
	 * file prints the values during its rendering process. update and bulkload
	 * is a two stage process. In the first stage GAF file parsed and annotation
	 * rules are run. A summary of rules voilation is represented to the user.
	 * The second stage is commit which makes changes in the database.
	 * 
	 * This service performs some of the tasks in multiple steps. The first step
	 * for the task except the 'getlastupdate' is to load gaf documents. It
	 * tries to load three locations: 1)system default location, 2) remote
	 * location (http/ftp urls) and (3) through the filelocation parameter.
	 */
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		try {

			command = request.getParameter("command");
			update = "update".equals(request.getParameter("command"));
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
			}
			/**
			 * The below condition sets gaf documents location
			 */
			else if (runner == null && !complete) {

				if (remoteGAF != null) {
					gafLocations = new GafURLFetch(remoteGAF);
				} else if (fileLocation != null) {
					gafLocations = new ArrayList<String>();
					((ArrayList) gafLocations).add(fileLocation);
				} else {

					this.gafLocations = GeneOntologyManager.getInstance()
							.getDefaultGafFileLocations();

				}
				
				if(!(update && updateInProgress)){
					runner = new GafDbTaskExecution();
					runner.start();
				}
				
				if(update)
					updateInProgress = true;

			}

		} finally {

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
			if (runner != null && !runner.isRunning()) {
				runner = null;
			}

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
	class GafDbTaskExecution extends Task implements DbOperationsListener {

		// id of the current
		private String currentOntologyBeingProcessed;

		// private Exception exception;
		GAFDbOperations db = null;

		public GafDbTaskExecution() {
			this.data = this;
			db = new GAFDbOperations();
			db.addDbOperationsListener(this);
		}

		/*
		private void runCommand(GafDocument gafDocument) throws Exception{
			
			
			if (runAnnotationRules) {

				currentOntologyBeingProcessed = gafDocument
						.getDocumentPath();
				reportStartTime("Performing Annotations Checks--"
						+ currentOntologyBeingProcessed);
				_performAnnotationChecks(gafDocument);
				reportEndTime("Performing Annotations Checks--"
						+ currentOntologyBeingProcessed);

				buildSplittedDocuments();

				LOG.info("Annotations checks completed.");

			} else

			if (solrLoad) {

				GafSolrLoader loader = new GafSolrLoader(
						GeneOntologyManager.getInstance().getSolrUrl());

					currentOntologyBeingProcessed = gafDocument
							.getDocumentPath();
					reportStartTime("Loading into Solr--"
							+ currentOntologyBeingProcessed);
	
					loader.load(gafDocument);
	
					reportEndTime("Loading into Solr--"
							+ currentOntologyBeingProcessed);


				buildSplittedDocuments();
				gafDocuments = null;
				splittedDocuments = null;

			} else if (update) {
				this.currentOntologyBeingProcessed = gafDocument
						.getId();

				db.update(gafDocument);
				buildSplittedDocuments();

			}else{
				buildSplittedDocuments();
			}
		}*/
		
		@Override
		public void execute() {


			try {


				if (gafLocations instanceof GafURLFetch) {
					GafURLFetch fetch = (GafURLFetch) gafLocations;
					fetch.connect();
					while (fetch.hasNext()) {

						
						try{
							InputStream is = (InputStream) fetch.next();
							java.io.Reader reader = new InputStreamReader(is);
							buildGafDocument(reader,
									fetch.getCurrentGafFile(),
									fetch.getCurrentGafFilePath());
								
								//gafDocuments.add(doc);
							reader.close();
							is.close();
							fetch.completeDownload();
						}catch (Exception ex) {
							this.exception = ex;
							LOG.error(ex, ex);
						}
					}
				} else {

					List<String> files = (List<String>) gafLocations;
					for (String ontLocation : files) {

						File file = null;

						if (ontLocation.startsWith("http:/")
								|| ontLocation.startsWith("file:/")) {
							file = new File(new URI(ontLocation));
						} else
							file = new File(ontLocation);

						File dirFiles[] = null;

						if (file.isDirectory()) {
							dirFiles = file.listFiles();
						} else {
							dirFiles = new File[] { file };
						}

						for (File f : dirFiles) {
							if (!f.isFile() || f.getName().startsWith("."))
								continue;
							ontLocation = f.getAbsolutePath();
							this.currentOntologyBeingProcessed = ontLocation;

							
							try{
								InputStream is = new FileInputStream(f);
	
								if (f.getName().endsWith(".gz")) {
									is = new GZIPInputStream(is);
								}
	
								buildGafDocument(
										new InputStreamReader(is), f.getName(),
										ontLocation);
							}catch(Exception ex){
								this.exception = ex;
								LOG.error(ex, ex);
							}
								//gafDocuments.add(doc);

						}
					}

				}



			} catch (Throwable ex) {
				this.exception = ex;
				LOG.error(ex, ex);

			}finally{
				//gafDocuments = null;
				//splittedDocuments = null;
				complete = true;
				updateInProgress = false;
			}
		}

		private void _performAnnotationChecks(GafDocument doc) {
			LOG.info("Performing Annotation Checks");
			AnnotationRulesEngine engine = AnnotationRulesEngine.getInstance();

			Set<AnnotationRuleViolation> violations = engine
					.validateAnnotations(doc);

			annotationRuleViolations.addAll(violations);

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
			reportStartTime("Parsing GAF--" + currentOntologyBeingProcessed);
		}

		public void endOntologyLoad(Object object) {
			reportEndTime("Parsing GAF--" + currentOntologyBeingProcessed);

			if (object instanceof GafObjectsBuilder) {
				GafObjectsBuilder builder = (GafObjectsBuilder) object;
				// gafDocuments.add(builder.getGafDocument());

				annotationRuleViolations.addAll(builder.getParser()
						.getAnnotationRuleViolations());

			}

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
			this.currentOntologyBeingProcessed = path;

			startOntologyLoad();

			GafObjectsBuilder builder = new GafObjectsBuilder();

			GafDocument doc = builder.buildDocument(reader, docId, path);

			GafDocument d = doc;

			/*GAFDbOperations db = null;
			if (update) {
				db = new GAFDbOperations();
				db.addDbOperationsListener(this);
			}*/

			int splitSize = GeneOntologyManager.getInstance().getSplitSize() * 4;
			do {
				if(d == null)
					break;
				
				currentOntologyBeingProcessed = d.getDocumentPath();

				if (annotationRuleViolations.size() >= splitSize) {
					throw new Exception(
							"The annotations check is terminated as the annotations voilations messages are being consumed.");
				}

				LOG.info("Splitting the '" + path + "' document");

				/*if (doc != null && !splittedDocuments.contains(path)) {
					splittedDocuments.add(path);
					// isLargefile = true;
				}*/

				if (update) {
					db.update(d);
				}

				else if (solrLoad) {
					GafSolrLoader loader = new GafSolrLoader(
							GeneOntologyManager.getInstance().getSolrUrl());

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

			endOntologyLoad(builder);

		}

	}

}
