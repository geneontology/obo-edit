package org.geneontology.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.OntologyBulkLoader;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFileLoader;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import owltools.graph.OWLGraphWrapper;

/**
 * Servlet implementation class AdminServlet
 */
public class AdminServlet extends HttpServlet{
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public AdminServlet() {
		super();
		// TODO Auto-generated constructor stub
	}

	
	private void printTaskStatus(DbOperationsTask task, Writer writer, HttpSession session, boolean addReload) 
		throws IOException{
		if(task.isRunning() || addReload){
			writer.write("<script type='text/javascript'>");
				writer.write("setTimeout(\"location.reload(true)\", 9000)");
			writer.write("</script>");
			
			writer.write("<center><img src=\"../images/progress-indicator.gif\" alt=\"Request is in Progress\" /></center>");
			writer.write("<p align=\"center\">Your Request is in Progress</p>");
		}

		
		writer.write("<h2>Status report for the running task '" + task.getOperationName() + "' :</h2>");
		writer.write("<table><tr><th>Operation Name</th><th>Status</th></tr>");
		Exception ex = task.getException();
		 for(String opName: task.getCompletedOperations()){
			 long stTime = task.getStartTime(opName);
			 long endTime = task.getEndTime(opName);
			 
			 String status = "In progress";
			 boolean isCompleted = false;
			 if(endTime>0){
				 status = (float)(endTime - stTime)/1000 + "";
				 isCompleted = true;
			 }else if (ex != null){
				 status = "failed";
			 }
			 
			 writer.write("<tr><td>" + opName + "</td><td" + (isCompleted ? " bgcolor='green'" : "") + ">" + status + "</td></tr>");
		 }
		 
		 writer.write("</table>");
		
		 
		 if(task.getException() != null){
			PrintWriter pw = new PrintWriter(writer);
			task.getException().printStackTrace(pw);
		 }
		 
		 if(!task.isRunning() && !addReload){
			 session.removeAttribute("task");
		 }
		
	}
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {

		
		HttpSession session = request.getSession();
		
		Object obj = session.getAttribute("task");
		
		Writer writer = response.getWriter();
		writer.write("<html>");
		writer.write("<head><title>");
		writer.write("GOLD Admin Operation Status");
		writer.write("</title></head>");
		
		String command = request.getParameter("command");
		
		
		boolean addReload = false;
		if (command != null && obj == null) {
			if ("bulkload".equals(command)) {
				addReload = true;
				DbOperationsTask task = new DbOperationsTask(command);
				session.setAttribute("task", task);
				obj = task;
				task.start();
				 
			}else if ("update".equals(command)) {
				addReload = true;
				DbOperationsTask task = new DbOperationsTask(command);
				session.setAttribute("task", task);
				obj = task;
				task.start();
			}else if ("getlastupdate".equals(command)){
				DbOperations db = new DbOperations();

				writer.write("<h1>Ontology update history</h1>");
				for(Ontology ont: db.getLastUpdateStatus()){

					writer.write("<hr />");
					
					writer.write("Ontology          : " + ont.getLabel());
					writer.write("<br >Version           : " + ont.getVersioniri());
					writer.write("<br />Laste Update Date : " + ont.getCreationDate());
					
				}
			}

		}else if(obj == null){
			writer.write("<h1>No valid parameters are provided. Please call this page with valid parameters</h1>");
			return;
		}
		
		DbOperationsTask task = (DbOperationsTask) obj;
		
		if(task != null){

			try{
				Thread.sleep(1000);
			}catch(Exception ex){
				
			}
		
			printTaskStatus(task, writer, session, addReload);
		}
		

		/*if(task.isRunning() || addReload){
			writer.write("<script type='text/javascript'>");
				writer.write("setTimeout(\"location.reload(true)\", 9000)");
			writer.write("</script>");
			
			writer.write("<center><img src=\"../images/progress-indicator.gif\" alt=\"Request is in Progress\" /></center>");
			writer.write("<p align=\"center\">Your Request is in Progress</p>");
		}

		
		writer.write("<h2>Status report for the running task '" + task.getOperationName() + "' :</h2>");
		writer.write("<table><tr><th>Operation Name</th><th>Status</th></tr>");
		Exception ex = task.getException();
		 for(String opName: task.getCompletedOperations()){
			 long stTime = task.getStartTime(opName);
			 long endTime = task.getEndTime(opName);
			 
			 String status = "In progress";
			 boolean isCompleted = false;
			 if(endTime>0){
				 status = (float)(endTime - stTime)/1000 + "";
				 isCompleted = true;
			 }else if (ex != null){
				 status = "failed";
			 }
			 
			 writer.write("<tr><td>" + opName + "</td><td" + (isCompleted ? " bgcolor='green'" : "") + ">" + status + "</td></tr>");
		 }
		 
		 writer.write("</table>");
		
		 
		 if(task.getException() != null){
			PrintWriter pw = new PrintWriter(writer);
			task.getException().printStackTrace(pw);
		 }
		 
		 if(!task.isRunning() && !addReload){
			 session.removeAttribute("task");
		 }*/
		
		writer.write("</body></html>");
	}

	
	
	
	

}
