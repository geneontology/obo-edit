package org.geneontology.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Hashtable;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.OntologyBulkLoader;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.geneontology.web.services.ServiceHandler;
import org.geneontology.web.services.ServicesConfig;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import owltools.graph.OWLGraphWrapper;

/**
 * Servlet implementation class AdminServlet
 */
public class AdminServlet extends HttpServlet {
	
	private void printError(String message, PrintWriter writer){
		writer.println("<html><body>");
		writer.println("<center><h1>************"+ message +"************</h1></center>");
		writer.println("</body></html>");
		
	}
	
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {

	
		String servicename= request.getParameter("servicename");
		
		if(servicename == null){
			printError("servicename parameter is missing in the parameter", response.getWriter());
			return;
		}
		
		ServiceHandler handler= ServicesConfig.getService(servicename);	
		
		if(handler == null){
			printError("The service '"+ servicename + "' is not supproted by the server", response.getWriter());
		}
		
		handler.handleService(request, response);
		
		if(handler.getViewPath() != null){
			//RequestDispatcher dispatcher = getServletContext().getContext(handler.getViewPath()). .getRequestDispatcher(handler.getViewPath());

			ServletContext context = getServletContext().getContext("/");
			RequestDispatcher dispatcher = context.getRequestDispatcher(handler.getViewPath());
			
			dispatcher.forward(request, response);
		}
		
	}

}
