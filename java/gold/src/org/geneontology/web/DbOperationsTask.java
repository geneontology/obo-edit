package org.geneontology.web;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GafObjectsBuilder;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gold.io.DbOperations;
import org.geneontology.gold.io.DbOperationsInterface;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.geneontology.web.services.DbOperationsService;
import org.geneontology.web.services.ServicesConfig;
import org.obolibrary.obo2owl.Owl2Obo;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory;

import owltools.InferenceBuilder;
import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty.Quantifier;

@Deprecated
public class DbOperationsTask extends Task implements DbOperationsListener {

	private static Logger LOG = Logger.getLogger(DbOperationsTask.class);

	private String opName;

	// private DbOperations db;

	// private OWLGraphWrapper []graphs;

	private String[] locations;

	private boolean force;

	private String tablePrefix;

	private String tsvFileDir;
 
	// id of the current
	private String currentOntologyBeingProcessed;

	// private List<OWLGraphWrapper> graphs;

	private OWLGraphWrapper graph;

	private List<GafDocument> gafDocuments;

	private String dbType;

	private Set<AnnotationRuleViolation> annotationRuleViolations;

	private StringBuffer reasonerComputationsResults;
	
	
	public DbOperationsTask(String op, String[] locations, boolean force,
			String tablePrefix, String tsvFilesDir) {
		this("gold", op, locations, force, tablePrefix, tsvFilesDir);
	}

	public DbOperationsTask(String dbType, String op, String[] locations,
			boolean force, String tablePrefix, String tsvFilesDir) {
		super();

		this.dbType = dbType;
		this.opName = op;
		this.locations = locations;
		this.force = force;
		this.tsvFileDir = tsvFilesDir;
		// graphs = new Vector<OWLGraphWrapper>();
		gafDocuments = new ArrayList<GafDocument>();

		annotationRuleViolations = new HashSet<AnnotationRuleViolation>();

		if ("checkconsistency".equals(opName)
				|| "find-inferences".equals(opName))
			this.reasonerComputationsResults = new StringBuffer();

	}

	public DbOperationsTask(String op) throws OWLOntologyCreationException,
			IOException {
		// this(op, new DbOperations().buildOWLGraphWrapper(), false, "", "");
		this(op, null, false, "", "");

	}

	public void setOWLGrapWrapper(OWLGraphWrapper wrapper) {
		this.graph = wrapper;
	}

	@Override
	public void run() {
		LOG.info("Running Db operation : " + opName);

		this.exception = null;
		running = true;
		DbOperationsInterface db = null;
		OWLGraphWrapper graph = null;
		if ("gaf".equals(this.dbType)) {

			DbOperationsService dbservice = (DbOperationsService) ServicesConfig
					.getService("db-operations");
			if (dbservice != null) {
				graph = dbservice.getOntologyGraph();
			}
			db = new GAFDbOperations();

		} else
			db = new DbOperations();

		db.addDbOperationsListener(this);

		try {
			boolean bulkLoadDone = false;

			if ("checkconsistency".equals(opName)) {
				// performConsistancyCheck(((DbOperations)
				// db).buildOWLGraphWrapper(location).getSourceOntology());
				performConsistancyCheck(this.graph.getSourceOntology());
			} else if ("find-inferences".equals(opName)) {
				// performConsistancyCheck(db.buildOWLGraphWrapper(location).getSourceOntology());
				// findInferences(((DbOperations)db).buildOWLGraphWrapper(location));
				findInferences(this.graph);
			}

			else {
				for (String location : locations) {
					this.currentOntologyBeingProcessed = location;
					if ("bulkload".equals(opName)) {
						if (db instanceof GAFDbOperations && graph != null) {
							db.bulkLoad(location, force);
							for (GafDocument doc : gafDocuments) {

								annotationRuleViolations.addAll(doc
										.validateAnnotations(graph));
								/*
								 * for(AnnotationRuleViolation arv:
								 * doc.validateAnnotations(graph)){
								 * this.annotationRuleViolations
								 * .append(arv.getMessage() + "----" +
								 * arv.getSourceAnnotation()); }
								 */
							}
						} else {
							if (bulkLoadDone) {
								db.update(location);
							} else {
								db.bulkLoad(location, force);
								bulkLoadDone = true;
							}
						}
					} else if ("update".equals(opName)) {
						db.update(location);
					} else if ("buildschema".equals(opName)) {
						db.buildSchema(force, tablePrefix);
					} else if ("buildtsv".equals(opName)) {
						db.dumpFiles(tablePrefix, location);
					} else if ("loadtsv".equals(opName)) {
						db.loadTsvFiles(tsvFileDir);
					}/*
					 * else if("checkconsistency".equals(opName)){ //
					 * performConsistancyCheck(((DbOperations)
					 * db).buildOWLGraphWrapper(location).getSourceOntology());
					 * performConsistancyCheck(this.graph.getSourceOntology());
					 * }else if("find-inferences".equals(opName)){
					 * //performConsistancyCheck
					 * (db.buildOWLGraphWrapper(location).getSourceOntology());
					 * //findInferences(((DbOperations)db).buildOWLGraphWrapper(
					 * location)); findInferences(this.graph); }
					 */
				}

			}
		} catch (Exception e) {
			running = false;
			this.exception = e;
			e.printStackTrace();
			LOG.error("DB Operation failed " + opName, e);
		} finally {
			running = false;
		}

	}

	public String getOperationName() {
		return opName;
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
			if (graph == null) {
				try {
					graph = new OWLGraphWrapper(ont);
				} catch (Exception ex) {
					LOG.error(ex.getMessage(), ex);
					this.exception = ex;
				}
			} else {
				graph.addSupportOntology(ont);
			}
		} else if (object instanceof GafObjectsBuilder) {
			GafObjectsBuilder builder = (GafObjectsBuilder) object;
			gafDocuments.add(builder.getGafDocument());

			this.annotationRuleViolations.addAll(builder.getParser()
					.getAnnotationRuleViolations());

			/*for (String s : builder.getParser().getErrors()) {
				this.annotationRuleViolations
						.add(new AnnotationRuleViolation(s));
			}*/

		}
	}

	/*
	 * public List<OWLGraphWrapper> getGraphs(){ return graphs; }
	 */

	public OWLGraphWrapper getOWLGraphWrapper() {
		return this.graph;
	}

	public String getReasonerComputationsResults() {
		return reasonerComputationsResults == null ? null : reasonerComputationsResults
				.toString();
	}

	public Set<AnnotationRuleViolation> getAnnotationVoilations() {
		return this.annotationRuleViolations;
	}


	private void performConsistancyCheck(OWLOntology ontology) {

		long start = System.currentTimeMillis();
		PelletReasonerFactory factory = new PelletReasonerFactory();
		OWLReasoner reasoner = factory.createReasoner(ontology);

		reasonerComputationsResults.append("<h1>Consistency Checks Results</h1>");

		reasonerComputationsResults
				.append("<h2>Performing Consitancy Check for ontology :"
						+ Owl2Obo.getIdentifier(ontology.getOntologyID()
								.getOntologyIRI()) + "</h2>");

		boolean isConsistent = reasoner.isConsistent();

		reasonerComputationsResults.append("is Consistent: " + isConsistent
				+ "<br />");

		Node<OWLClass> unsatisfiableClasses = reasoner
				.getUnsatisfiableClasses();
		if (unsatisfiableClasses.getSize() > 0) {
			reasonerComputationsResults
					.append("<br /><br /><h3>The following classes are unsatisfiable: </h3><br />");
			for (OWLClass cls : unsatisfiableClasses) {
				if (cls.toString().endsWith("Nothing"))
					continue;
				reasonerComputationsResults
						.append("&nbsp;&nbsp;&nbsp;-unsatisfiable: "
								+ Owl2Obo.getIdentifier(cls) + "<br />");
			}
		} else {
			reasonerComputationsResults
					.append("There are no unsatisfiable classes<br />");
		}
		
		long end = System.currentTimeMillis();
		
		reasonerComputationsResults.append("<hr />");
		reasonerComputationsResults.append("Total time taken to compute the consistency check in seconds is: " + (end-start)/1000);

	}


	
	/*public List<OWLGraphEdge> infEdges;
	public OWLOntology infOntology;
	
	private OWLOntology dubplicate(OWLGraphWrapper graph) throws OWLOntologyCreationException{
		OWLOntology ontology =graph.getManager().createOntology(graph.getSourceOntology().getOntologyID().getOntologyIRI());
		
		OWLOntologyManager manager = graph.getManager();
		for(OWLAxiom ax: graph.getSourceOntology().getAxioms()){
			manager.applyChange(new AddAxiom(ontology, ax));
			
		}
		
		return ontology;
	}*/
	
//	public InferenceBuilder infBuilder;
	
 	private void findInferences(OWLGraphWrapper graph) {
	
		long start = System.currentTimeMillis();
		
		reasonerComputationsResults
				.append("<h1>Inferences Computations Results</h1>");

	/*	if(infBuilder == null)
			infBuilder = new InferenceBuilder(graph);
		
		List<OWLAxiom> edges= infBuilder.buildInferences();*/
		/*infEdges = edges;
		
		try{
			this.infOntology = dubplicate(graph);
			
			
		}catch(Exception ex){
			
		}
		OWLDataFactory factory = graph.getDataFactory();
		OWLOntologyManager manager = graph.getManager();
		*/
		reasonerComputationsResults
		.append("<table><tr><th>Axiom Type</th><th>Class 1</th><th>Classs 2</th><th></th></tr>");
		
		int totalSubclass = 0;
		int totalEq = 0;
		/*
		for(OWLGraphEdge edge: edges){
			IRI cls1 = ((OWLClass)edge.getSource()).getIRI();
			IRI cls2 = ((OWLClass)edge.getTarget()).getIRI();
			String quantifier = edge.getSingleQuantifiedProperty().getQuantifier().toString();
			String id = UUID.randomUUID().toString();
		
			
			if(edge.getSingleQuantifiedProperty().getQuantifier() == Quantifier.EQUIVALENT){
				totalEq++;
			}else
				totalSubclass++;
			
			reasonerComputationsResults.append("<tr><td>" + quantifier + "</td>");
			reasonerComputationsResults.append("<td>" +  Owl2Obo.getIdentifier(cls1) + "[" + graph.getLabel(edge.getSource()) + "]</td>");
			reasonerComputationsResults.append("<td>" +  Owl2Obo.getIdentifier(cls2) + "[" + graph.getLabel(edge.getTarget()) + "]</td>");
			reasonerComputationsResults.append("<td><a href='javascript:getExplanation(\""+cls1 + "\",\""+ cls2 + "\", \"" + quantifier + "\", \"" + id + "\" );'>Explanation</a></td>");
			reasonerComputationsResults.append("</tr>");
			reasonerComputationsResults.append("<tr class='explanation'><td colspan='4'><div id='" +id+ "'></div></td></tr>");
		}*/
		
		reasonerComputationsResults.append("</table>");
		
		/*OWLOntology ontology = graph.getSourceOntology();
		PelletReasonerFactory factory = new PelletReasonerFactory();
		OWLReasoner reasoner = factory.createReasoner(ontology);

		Set<OWLClass> nrClasses = new HashSet<OWLClass>();

		for (OWLClass cls : ontology.getClassesInSignature()) {
			if (nrClasses.contains(cls))
				continue; // do not report these

			// REPORT INFERRED EQUIVALENCE BETWEEN NAMED CLASSES
			for (OWLClass ec : reasoner.getEquivalentClasses(cls)) {
				if (nrClasses.contains(ec))
					continue; // do not report these

				if (cls.toString().equals(ec.toString()))
					continue;

				if (cls.toString().compareTo(ec.toString()) > 0) // equivalence
																	// is
																	// symmetric:
																	// report
																	// each pair
																	// once
					reasonerComputationsResults
							.append("&nbsp;&nbsp;&nbsp;* INFERRED: equivalent "
									+ getLabel(cls, graph) + " "
									+ getLabel(ec, graph) + "<br />");
			}

			// REPORT INFERRED SUBCLASSES NOT ALREADY ASSERTED

			NodeSet<OWLClass> scs = reasoner.getSuperClasses(cls, true);
			for (Node<OWLClass> scSet : scs) {
				for (OWLClass sc : scSet) {
					if (sc.toString().endsWith("Thing")) {
						continue;
					}
					if (nrClasses.contains(sc))
						continue; // do not report subclasses of owl:Thing

					// we do not want to report inferred subclass links
					// if they are already asserted in the ontology
					boolean isAsserted = false;
					for (OWLClassExpression asc : cls.getSuperClasses(ontology)) {
						if (asc.equals(sc)) {
							// we don't want to report this
							isAsserted = true;
						}
					}
					for (OWLClassExpression ec : cls
							.getEquivalentClasses(ontology)) {

						if (ec instanceof OWLObjectIntersectionOf) {
							OWLObjectIntersectionOf io = (OWLObjectIntersectionOf) ec;
							for (OWLClassExpression op : io.getOperands()) {
								if (op.equals(sc)) {
									isAsserted = true;
								}
							}
						}
					}
					if (!isAsserted) {
						this.reasonerComputationsResults
								.append("&nbsp;&nbsp;&nbsp;&nbsp;*INFERRED:  "
										+ getLabel(cls, graph) + " subClassOf "
										+ getLabel(sc, graph) + "<br />");
					}
				}
			}
		}*/
		
		long end = System.currentTimeMillis();
		
		this.reasonerComputationsResults.append("<hr />");
		
		this.reasonerComputationsResults.append("<h2>Total Time to computer inferences in seconds is : " + (end-start)/1000 + "</h2>");
		this.reasonerComputationsResults.append("<h3>Total Sub Classes : " + totalSubclass + "</h3>");
		this.reasonerComputationsResults.append("<h3>Total Equivalent Classes : " + totalEq + "</h3>");

	}

	@Override
	public void execute() {
		// TODO Auto-generated method stub
		
	}

	/*private static String getLabel(OWLClass cls, OWLGraphWrapper graph) {
		return "[" + Owl2Obo.getIdentifier(cls) + " ! " + graph.getLabel(cls) + "]";
	}*/

}
