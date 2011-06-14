package org.geneontology.web.services;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gold.hibernate.factory.GoldObjectFactory;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.FileMonitorListener;
import org.geneontology.web.Task;
import org.semanticweb.owlapi.model.OWLOntology;

import owltools.graph.OWLGraphWrapper;


/**
 * This class performs bulkload and update operations for geneontology GOLD database against the
 * web based requests.
 *  
 * @author Shahid Manzoor
 *
 */
public class GoldDbOperationsService extends ServiceHandlerAbstract{

	private static Logger LOG = Logger.getLogger(GoldDbOperationsService.class);
	
	
	public static final String SERVICE_NAME="gold-db-operations";
	
	private static FileMonitorListenerImp fileMonitor = new FileMonitorListenerImp();
	
	/**
	 * this instance is shared with the other service
	 */
	private static OWLGraphWrapper ontologyGraph = buildOWLGraphWrapper();

	/**
	 * This reference holds taxanomies configured through the geneontology.gold.taxonomylocation
	 * property in the conf/gold.properties file.
	 */
	private static OWLGraphWrapper taxonomyGraph = buildTaxonomyGraph();
	
	/**
	 * Path of the jsp file which renders the results of the computations of this service.
	 */
	private String viewPath;
	
	/**
	 * The ontologies files paths
	 */
	private List<String> ontLocations;

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
	
	private String command;
	
	private static OWLGraphWrapper buildTaxonomyGraph(){
		return buildOWLGraphWrapper(GeneOntologyManager.getInstance().getTaxonomiesLocations());
	}
	
	public GoldDbOperationsService(){
		
		
			//ontologyGraph = buildOWLGraphWrapper();
			//taxonomyGraph = buildOWLGraphWrapper(GeneOntologyManager.getInstance().getTaxonomiesLocations());
		runner = null;
	}
	
	/**
	 * This service performs the computations and stores the computations results via request.setAttribute method.
	 * This request object attributes values are visible in the jsp file associated with this service. 
	 * The jsp file prints the values during its rendering process.   
	 */
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		command = request.getParameter("command");
		//this.force = "true".equals(request.getParameter("force"));
	
		//set the default view
		viewPath = "/servicesui/golddb.jsp";
		//if there is no task running then create one for the update and bulkload commands
		if(runner == null){
			GoldObjectFactory factory = GoldObjectFactory.buildDefaultFactory();
			List list = factory.getOntologies();
			if ("update".equals(command) && !list.isEmpty()) {
					String ontologylocation[] = request
							.getParameterValues("filelocation");
					
					if(ontologylocation != null){
						ontLocations = new ArrayList<String>();
						for(String l: ontologylocation)
							ontLocations.add(l);
						runner = new GoldDbTaskExecution();
					}else{
						request.setAttribute("servicename", getServiceName());
						request.setAttribute("locations", GeneOntologyManager.getInstance().getDefaultOntologyLocations());
						this.viewPath = "/servicesui/golddb-updateform.jsp";
					}
			}else if("bulkload".equals(command) || ("update".equals(command) && list.isEmpty()) ){
				this.ontLocations = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
				runner = new GoldDbTaskExecution();
				command = "bulkload";
			}else if("getlastupdate".equals(command)){

				viewPath = "/servicesui/gold-lastupdate.jsp";
				
				DbOperations db = new DbOperations();
				request.setAttribute("dbname", "Gold");
				Hashtable<String, String[]> dbs = new Hashtable<String, String[]>();
				request.setAttribute("dbs", dbs);
				
				for(Ontology ont: db.getLastUpdateStatus()){
					dbs.put(ont.getId(), new String[]{ont.getVersioniri(), ont.getCreationDate()});
				}
				
			}
		

			if(this.runner != null){
				ontologyGraph = null;
				runner.start();
			}
		}
	
		//store information in the request object. The request object is available in the 
		//jsp file. The jsp use the objects data in print html.
		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		request.setAttribute("dbname", "GOLD");
		
		if(runner != null){
			request.setAttribute("task", runner.getData());
		}
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}
		
		
	}
	
	public synchronized static  OWLGraphWrapper getTaxonomiesGraph(){
		return taxonomyGraph;
	}
	
	public synchronized static OWLGraphWrapper getGraphWrapper(){
		return ontologyGraph;
	}

	public String getServiceName() {
		// TODO Auto-generated method stub
		return SERVICE_NAME;
	}

	public String getViewPath(){
		return this.viewPath;
	}
	
	private static OWLGraphWrapper buildOWLGraphWrapper(){

		List ontologies = GeneOntologyManager.getInstance().getDefaultOntologyLocations();
		
		return buildOWLGraphWrapper(ontologies);
	}
	

	private static OWLGraphWrapper buildOWLGraphWrapper(List ontologies){
		
		DbOperations db = new DbOperations();
		OWLGraphWrapper wrapper = null;
		for(int i=0;i<ontologies.size();i++){
			
			String location = ontologies.get(i).toString();

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
				
	//			if(ontologyGraph == null)
		//			ontologyGraph = wrapper;
				//graphs.put(wrapper.getOntologyId(), wrapper); 
			}catch(Exception ex){
				LOG.error(ex.getMessage(), ex);
			}
		}
		
		return wrapper;
		
		
		
	}
	
	
//	class GoldDbTaskExecution extends Task implements TaskExecution, DbOperationsListener{
	/**
	 * The execute method is called by the {@link Task} class. This class executes the
	 * update and bulkload methods of {@link DbOperations}. The implemented listener  methods of the
	 * {@link DbOperationsListener} interface are called {@link GAFDbOperations}. The listener methods keep
	 * stores start and end time of sub task of the the bulkload and update operation. The subtasks completion
	 * time is printed by the jsp associated with the GoldDbOperationsService class. 
	 * 
	 * @author Shahid Manzoor
	 *
	 */
	class GoldDbTaskExecution extends Task implements DbOperationsListener{

		// id of the current ontology being proce
		private String currentOntologyBeingProcessed;
		
		
		public GoldDbTaskExecution(){
		}
		
		
		@Override
		public void execute() {
			// TODO Auto-generated method stub
		
			this.data = this;
			
			DbOperations db = new DbOperations();
			db.addDbOperationsListener(this);
			
			try{
					OWLGraphWrapper wrapperHolder = null;
					for(String ontLocation: ontLocations){
						this.currentOntologyBeingProcessed = ontLocation;
						OWLGraphWrapper graph = db.buildOWLGraphWrapper(ontLocation);
						
						if(wrapperHolder == null){
							wrapperHolder = new OWLGraphWrapper(graph.getSourceOntology());
						}else{
							wrapperHolder.addSupportOntology(graph.getSourceOntology());
							wrapperHolder.mergeOntology(graph.getSourceOntology());
						}
						
						if("bulkload".equals(command))
							db.bulkLoad(graph, force);
						else 
							db.updateGold(graph);
					}
				if("update".equals(command)){
					//rebuilt the merge because of change in one of the ontology
					if(wrapperHolder != null)
						ontologyGraph = wrapperHolder;
					
					
				}
			}catch(Exception ex){
				this.exception = ex;
				LOG.error(ex, ex);
			}
			
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

	private static class FileMonitorListenerImp implements FileMonitorListener{
		/**
		 * Rebuild the ontologyGraph when ontology files are modified
		 */
		public void filesModified(String[] files) {
			LOG.info("filesModified event occured");

			
			ontologyGraph = buildOWLGraphWrapper();	
			
			try{
				DbOperations db = new DbOperations();
				for(String file: files){
					db.update(file);
				}
			}catch(Exception ex){
				LOG.error(ex.getMessage(), ex);
			}
		
			
		}
		
	}
	
	
	public static FileMonitorListener getFileMonitorListener(){
		return fileMonitor;
	}
	
}
