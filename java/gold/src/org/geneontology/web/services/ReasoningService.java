package org.geneontology.web.services;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.gold.io.reasoner.InferenceBuilder;
import org.geneontology.web.TaskRunner;
import owltools.graph.OWLGraphEdge;

public class ReasoningService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(ReasoningService.class);
	
	
	private InferenceBuilder inf;
	
	private TaskRunner runner;
	
	
	public ReasoningService(){
		//inf = new InferenceBuilder();
		inf = null;
		runner = null;
	}
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {


		LOG.info("*******************");
	
		String command = request.getParameter("command");

		if(runner == null && "find-inferences".equals(command)){
			inf = getInferenceBuilder();
			if(runner == null){
				runner = new TaskRunner(inf);
				runner.start();
			}
		}
		
		List<OWLGraphEdge> edges = null;
		
		
		
		if(runner != null){
			edges = (List<OWLGraphEdge>)runner.getData();
		}

		request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
		
		if(runner != null && !runner.isRunning()){
			runner = null;
		}

		if(edges == null){
			edges = new ArrayList<OWLGraphEdge>();
		}

		request.setAttribute("edges", edges);
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
		if(this.inf == null){
			DbOperationsService dbService =(DbOperationsService) ServicesConfig.getService("db-operations");
			inf = new InferenceBuilder(dbService.getOntologyGraph());
		}
		
		return inf;
	}
	
	
	
}
