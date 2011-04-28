package org.geneontology.web.services;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.solrj.OntSolrLoader;
import org.geneontology.web.Task;

/**
 * This service load ontology files into solr through solrj interface.
 * The {@link OntSolrLoader} class is wrapping implementation of solrj.
 * @author shahidmanzoor
 *
 */
public class OntSolrjLoaderService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(OntSolrjLoaderService.class);
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private Task runner;
	
	public OntSolrjLoaderService(){
		
	}
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		if(DEBUG)
			LOG.debug("--");

		
		if(runner == null){
			if(runner == null){
				runner = new SorjLoaderTask();
				runner.start();
			}
		}
		
		String completedTime = "--";
		if(runner != null){
			long st = runner.getStartTime("load-ontology");
			long et = runner.getEndTime("load-ontology");
			
			if(!(st == -1 || et <=0)){
				completedTime = (et - st)/1000 + "";
			}
		}

		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		request.setAttribute("taskCompletionTime", completedTime);
		request.setAttribute("task", runner);
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}

	}

	public String getViewPath(){
		return "/servicesui/solrj-load.jsp";
	}

	@Override
	public String getServiceName() {
		return "ont-solrj-loading-service";
	}
	
	class SorjLoaderTask extends Task{

		@Override
		public void execute() {

			try{
				this.addInProgress("load-ontology");
				OntSolrLoader loader = new OntSolrLoader(GeneOntologyManager.getInstance().getSolrUrl());
				loader.load();
				this.addCompleted("load-ontology");
			}catch(Exception ex){
				LOG.error(ex.getMessage(), ex);
				exception = ex;
			}
			
		}
		
		
		@Override
		public Object getData(){
			return this;
		}
		
	}
	

}
