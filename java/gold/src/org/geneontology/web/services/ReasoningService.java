package org.geneontology.web.services;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.web.Task;
import org.semanticweb.owlapi.model.OWLAxiom;
import owltools.InferenceBuilder;

public class ReasoningService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(ReasoningService.class);
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	/**
	 * The reference of the inference builder is kept alive
	 * until the life of the servlet. The inference builder cache
	 * the reasoner reference.
	 */
	private InferenceBuilder inf;
	
	private Task runner;
	
	
	private String command;
	
	private String viewPath;
	
	public ReasoningService(){
		inf = null;
		runner = null;
	}
	
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		if(DEBUG)
			LOG.debug("--");
		
	
		command = request.getParameter("command");

		viewPath = "/servicesui/reasoning.jsp";
		
		if("checkconsistency".equals(command)){
			viewPath = "/servicesui/consistencycheck.jsp";
		}
		
		if(runner == null){
			inf = getInferenceBuilder();
			if(runner == null){
				runner = new InferenceBuilderTask();
				runner.start();
			}
		}
		
		
		
		String completedTime = "--";
		if(runner != null ){

			long st = runner.getStartTime("processing-time");
			long et = runner.getEndTime("processing-time");
			
			if(!(st == -1 || et <=0)){
				completedTime = (et - st)/1000 + "";
			}
			request.setAttribute("taskCompletionTime", completedTime);
			request.setAttribute("graph", inf.getOWLGraphWrapper());
			
			
			if("find-inferences".equals(command)){
				List<OWLAxiom> axioms = null;
				axioms = (List<OWLAxiom>)runner.getData();
				if(axioms == null){
					axioms = new ArrayList<OWLAxiom>();
				}
	
				request.setAttribute("axioms", axioms);
			}else if("checkconsistency".equals(command)){
				request.setAttribute("errors", runner.getData());
			}
		}

		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}

	}

	public String getViewPath(){
		return viewPath;
	}
	
	
	public String getServiceName() {
		return "reasoning-service";
	}

	private InferenceBuilder getInferenceBuilder(){
		GoldDbOperationsService dbService =(GoldDbOperationsService) ServicesConfig.getService("gold-db-operations");
		if(this.inf == null){
			inf = new InferenceBuilder(dbService.getGraphWrapper());
		}else{
			//if the ontolgoy reference is changed then reset the ontology reference
			//reasoner reference in the InferenceBuilder class.
			if(inf.getOWLGraphWrapper() != dbService.getGraphWrapper()){
				inf.setOWLGraphWrapper(dbService.getGraphWrapper());
			}
		}
		
		return inf;
	}
	
	class InferenceBuilderTask extends Task{
		private Object data;

		@Override
		public void execute() {
			this.addInProgress("processing-time");
			
			if("find-inferences".equals(command)){			
				data = inf.buildInferences();
			}else if("checkconsistency".equals(command)){
				data = inf.performConsistencyChecks();
			}
			this.addCompleted("processing-time");
			
		}
		
		
		@Override
		public Object getData(){
			return data;
		}
		
	}
	
}
