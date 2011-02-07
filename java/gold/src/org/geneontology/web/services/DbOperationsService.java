package org.geneontology.web.services;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Hashtable;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.web.AdminServlet;
import org.geneontology.web.DbOperationsTask;

import owltools.graph.OWLGraphWrapper;

public class DbOperationsService extends ServiceHandlerAbstract {

	private static final Logger LOG = Logger.getLogger(AdminServlet.class);
	private static final boolean DEBUG = LOG.isDebugEnabled();

	// only task can be running at one time
	// task is not null then it means there
	// is a task running;
	private DbOperationsTask task;

	// global reference of the OWLGraphWrapper
	private Hashtable<String, OWLGraphWrapper> graphs;

	
	public DbOperationsService(){
		graphs = new Hashtable<String, OWLGraphWrapper>();
	}
	
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException {

		Writer writer = response.getWriter();
		writer.write("<html>");
		writer.write("<head><title>");
		writer.write("GOLD Admin Operation Status");
		writer.write("</title></head>");

		String command = request.getParameter("command");

		String dbType = request.getParameter("dbtype");
		
		if(dbType == null)
			dbType = "gold";
		
		
		
		
		if (DEBUG) {
			LOG.debug("Command parameter = " + command);
		}

		boolean addReload = false;
		if (command != null && task == null) {
			
			if ("update".equals(command)) {

				String ontologylocation = request
						.getParameter("ontologylocation");

				if (ontologylocation == null) {
					printOntologySelectionForm(writer, command);
				} else {

					/*
					 * OWLGraphWrapper graph =
					 * buildOWLGraphWrapper(ontologylocation);
					 * 
					 * if (graph == null) { writer.write(
					 * "<h1>OWLGraphWrapper cannot be built from the ontologylocation at '"
					 * + ontologylocation + "'"); } else {
					 * graphs.put(graph.getOntologyId(), graph);
					 */
					addReload = true;
					task = new DbOperationsTask(dbType , command,
							new String[] { ontologylocation }, false, "", "");
					task.start();
					// }
				}

			} else if ("bulkload".equals(command) ||  "checkconsistency".equals(command) || "find-inferences".equals(command)) {
				List list = GeneOntologyManager.getInstance()
						.getDefaultOntologyLocations();
				
				if("gaf".equals(dbType)){
					list = GeneOntologyManager.getInstance().getDefaultGafFileLocations();
				}
				
				String locations[] = new String[list.size()];
				list.toArray(locations);
				addReload = true;
				String force = request.getParameter("force");
				task = new DbOperationsTask(dbType, command, locations,
						"true".equals(force), "", "");
				task.start();

			} else if ("getlastupdate".equals(command)) {
				DbOperations db = new DbOperations();

				writer.write("<h1>Ontology update history</h1>");
				for (Ontology ont : db.getLastUpdateStatus()) {

					writer.write("<hr />");

					writer.write("Ontology          : " + ont.getLabel());
					writer.write("<br >Version           : "
							+ ont.getVersioniri());
					writer.write("<br />Laste Update Date : "
							+ ont.getCreationDate());

				}
			}

			try {
				Thread.sleep(1000);
			} catch (Exception ex) {

			}

		} else if (task == null) {
			writer.write("<h1>No valid parameters are provided. Please call this page with valid parameters</h1>");
			return;
		}

		// DbOperationsTask task = (DbOperationsTask) obj;

		if (task != null) {
			printTaskStatus(writer, addReload);
		}

		writer.write("</body></html>");
	}

	public String getServiceName() {
		return "db-operations";
	}

	private void printTaskStatus(Writer writer, boolean addReload)
			throws IOException {

		if (DEBUG) {
			LOG.debug("addReload variable: " + addReload);
			LOG.debug("Is Tasking Running: " + task != null ? task.isRunning()
					: null);
		}

		if (task.isRunning() || addReload) {
			writer.write("<script type='text/javascript'>");
			writer.write("setTimeout(\"location.reload(true)\", 9000)");
			writer.write("</script>");

			writer.write("<center><img src=\"../images/progress-indicator.gif\" alt=\"Request is in Progress\" /></center>");
			writer.write("<p align=\"center\">Your Request is in Progress</p>");
		}
		
		
		String consistencyResults = task.getConsistencyCheckResults();
		if(consistencyResults != null){
			writer.write("<h2>Consistency Check status</h2>");
			writer.write(consistencyResults);
		}else{
			writer.write("<h2>Status report for the running task '"
					+ task.getOperationName() + "' :</h2>");
			writer.write("<table><tr><th>Operation Name</th><th>Status/Completion Time</th></tr>");
			Exception ex = task.getException();
			String ontology = "";
			for (String opName : task.getCompletedOperations()) {
				String[] st = opName.split("--");
				String opLocalName = opName;
				if (st.length > 1) {
					if (!ontology.equals(st[1])) {
						ontology = st[1];
						writer.write("<tr><td colspan='2'><h4>Loading " + ontology
								+ "</h4></td></tr>");
					}
	
					opLocalName = st[0];
				}
	
				long stTime = task.getStartTime(opName);
				long endTime = task.getEndTime(opName);
	
				String status = "In progress";
				boolean isCompleted = false;
				if (endTime > 0) {
					status = (float) (endTime - stTime) / 1000 + "";
					isCompleted = true;
				} else if (ex != null) {
					status = "failed";
				}
	
				writer.write("<tr><td>" + opLocalName + "</td><td"
						+ (isCompleted ? " bgcolor='green'" : "") + ">" + status
						+ "</td></tr>");
			}
	
			writer.write("</table>");

		}
		
		if (task.getException() != null) {
			LOG.error("", task.getException());
			PrintWriter pw = new PrintWriter(writer);
			task.getException().printStackTrace(pw);
		}

		if (!task.isRunning() && !addReload) {
			for (OWLGraphWrapper graph : task.getGraphs()) {
				graphs.put(graph.getOntologyId(), graph);
			}
			this.task = null;
		}

	}

	/*public Collection<OWLOntology> getOntologies(){
		return graphs.values();
	}*/
	
	private void printOntologySelectionForm(Writer writer, String command)
			throws IOException {
		writer.write("<h1>Gene Ontology Admin</h1>");
		writer.write("<form action='.'>");
		writer.write("<input type='hidden' name='servicename' value='"+ this.getServiceName() +"' />");
		writer.write("<input type='hidden' name='command' value='" + command
				+ "' />");
		writer.write("<label>Please select ontology</label><br />");
		for (Object obj : GeneOntologyManager.getInstance()
				.getDefaultOntologyLocations()) {
			writer.write("<input type='radio' name='ontologylocation' value='"
					+ obj + "' />" + obj + "<br />");
		}
		// writer.write("<br /><label>Force to recreate the database</label>");
		// writer.write("<input type='checkbox' name='force' value='true' /><br /><br />");
		writer.write("<input type='submit' value='update' /><br />");
		writer.write("</form>");
	}
	
}
