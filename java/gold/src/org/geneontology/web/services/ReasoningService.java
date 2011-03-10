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
import owltools.graph.OWLGraphEdge;

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
	
	public ReasoningService(){
		inf = null;
		runner = null;
	}
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		if(DEBUG)
			LOG.debug("--");
		
	
		String command = request.getParameter("command");

		if(runner == null && "find-inferences".equals(command)){
			inf = getInferenceBuilder();
			if(runner == null){
				runner = new InferenceBuilderTask();
				runner.start();
			}
		}
		
		List<OWLAxiom> axioms = null;
		
		
		String completedTime = "--";
		if(runner != null){
			axioms = (List<OWLAxiom>)runner.getData();
			long st = runner.getStartTime("build-inferences");
			long et = runner.getEndTime("build-inferences");
			
			if(!(st == -1 || et <=0)){
				completedTime = (et - st)/1000 + "";
			}
		}

		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		request.setAttribute("taskCompletionTime", completedTime);
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}

		if(axioms == null){
			axioms = new ArrayList<OWLAxiom>();
		}

		request.setAttribute("axioms", axioms);
		request.setAttribute("graph", inf.getOWLGraphWrapper());
	}

	public String getViewPath(){
		return "/servicesui/reasoning.jsp";
	}
	
	
	@Override
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
		private List<OWLAxiom> edges;

		@Override
		public void execute() {
			this.addInProgress("build-inferences");
			edges = inf.buildInferences();
			this.addCompleted("build-inferences");
		}
		
		
		@Override
		public Object getData(){
			return edges;
		}
		
	}
	
}
