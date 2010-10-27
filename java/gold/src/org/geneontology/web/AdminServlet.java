package org.geneontology.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.OntologyBulkLoader;
import org.geneontology.gold.io.postgres.SchemaManager;
import org.geneontology.gold.io.postgres.TsvFilelLoader;
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
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public AdminServlet() {
		super();
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub

		Writer writer = response.getWriter();

		String command = request.getParameter("command");

		if (command != null) {

			if ("bulk-load".equals(command)) {

				// writer.write("<h1>Bulk load is command is provided. This ia temporary message. This message will be replaced as soon as this service is implemented.</h1>");
				// return;
				executeBulkLoad(writer);
				return;
			}
		}

		writer.write("<h1>No valid parameters are provided. Please call this page with valid parameters</h1>");

	}

	private void executeBulkLoad(Writer writer) throws IOException {
		/**
		 * The following code creates schema (database, tables) in the RDMBS if
		 * it is not created yet.
		 */

		//this object write exceptions as output on browser screen
		PrintWriter pw = new PrintWriter(writer);
		
		GeneOntologyManager manager = null;
		
		try{
			manager = GeneOntologyManager.getInstance();
		}catch(Exception ex){
			printFetalError(writer, "An Fetal Error Occured.");
			ex.printStackTrace(pw);
			
			return;
		}
		
		try {
			SchemaManager sm = new SchemaManager();
			sm.loadSchemaSQL(manager.getGolddbHostName(),
					manager.getGolddbUserName(),
					manager.getGolddbUserPassword(), manager.getGolddbName(),
					manager.getOntSqlSchemaFileLocation());

			printSucessMessage(writer, "Database schema is created successfully");
		} catch (Exception ex) {
			printWarning(writer, "Warning: If database already created then ignore the following exceptions");
			ex.printStackTrace(pw);
		}

		// transform obo files to tsv format
		String oboFile = null;
		try {

			oboFile = manager.getDefaultOboFile();
			OWLGraphWrapper wrapper = getGraphWrapper(oboFile);
			OntologyBulkLoader loader = new OntologyBulkLoader(wrapper, manager.getTsvFilesDir());
			loader.dumpBulkLoadTables();

			
			printSucessMessage(writer, "OBO file is dumped into TSV files successfully");
		} catch (Exception ex) {
			printFetalError(writer, "An error occured dumping OBO file '" +  oboFile + "' into TSV format");
			ex.printStackTrace(pw);
			
			return;
		}
		
		
		
		//load TSV files into database
		try{
			TsvFilelLoader tsvLoader = new TsvFilelLoader(manager.getGolddbUserName(),
					manager.getGolddbUserPassword(), manager.getGolddbHostName(), 
					manager.getGolddbName());
			
			tsvLoader.loadTables(manager.getTsvFilesDir());
			
			printSucessMessage(writer, "Bulk load of TSV files is succesful");

		}catch(Exception ex){
			printFetalError(writer, "An error occured during bulk load into the RDMBS");
			ex.printStackTrace(pw);
			
			return;
			
		}

	}

	
	private void printFetalError(Writer writer, String message) throws IOException{
		writer.write("<hr /> <br />");

		writer.write("<h1 style='color:red'>" + message+ "</h1>");

		writer.write("<br /> <hr />");
	
	}
	
	
	private void printWarning(Writer writer, String message) throws IOException{
		writer.write("<hr /> <br />");

		writer.write("<h3>" + message+ "</h3>");

		writer.write("<br /> <hr />");
	
	}
	
	private void printSucessMessage(Writer writer, String message) throws IOException{
		writer.write("<h2 style='color:green'>Success: " + message  +"</h2>");
	}
	
	
	private OWLGraphWrapper getGraphWrapper(String oboFile) throws IOException,
			OWLOntologyCreationException {

		OBOFormatParser p = new OBOFormatParser();
		OBODoc obodoc = p.parse(oboFile);

		Obo2Owl bridge = new Obo2Owl();
		OWLOntology ontology = bridge.convert(obodoc);

		return new OWLGraphWrapper(ontology);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
	}

}
