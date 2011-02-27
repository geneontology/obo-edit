package org.geneontology.web.services;

import java.io.IOException;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.geneontology.gold.io.reasoner.InferenceBuilder;
import org.semanticweb.owlapi.model.OWLAxiom;

import owltools.graph.OWLQuantifiedProperty.Quantifier;

import com.clarkparsia.owlapi.explanation.DefaultExplanationGenerator;

public class ReasoningService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(ReasoningService.class);
	
	
	private InferenceBuilder inf;
	
	
	public ReasoningService(){
		inf = new InferenceBuilder();
	}
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException {


		LOG.info("*******************");
		
		String cls1 = request.getParameter("cls1");
		String cls2 = request.getParameter("cls2");
		String relation = request.getParameter("quantifier");
		
		System.out.println("cls 1 " + cls1);
		System.out.println("cls 2 " + cls2);
		System.out.println("relation " + relation);
		
		
		DbOperationsService dbService =(DbOperationsService) ServicesConfig.getService("db-operations");
		
		Set<Set<OWLAxiom>> sets = dbService.infBuilder.getExplaination(cls1, cls2, Quantifier.valueOf(relation));
		
		
		
		response.getWriter().write("<p>Explanations</p>");
		
		for(Set<OWLAxiom> set: sets){
			
			for(OWLAxiom ax: set){
				System.out.println("Explanation: " + ax);
				
				response.getWriter().write(ax + "");
			}
		}
		
	//	DefaultExplanationGenerator exp = new ;
		
				
	}

	@Override
	public String getServiceName() {
		return "reasoning-service";
	}

}
