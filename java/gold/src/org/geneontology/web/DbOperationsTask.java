package org.geneontology.web;


import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory;

import owltools.graph.OWLGraphWrapper;

public class DbOperationsTask extends Task implements DbOperationsListener{

	private static Logger LOG = Logger.getLogger(DbOperationsTask.class);
	
	private String opName;
	
//	private DbOperations db;
	
	//private OWLGraphWrapper []graphs;
	
	private String[] locations;
	
	private boolean force;
	
	private String tablePrefix;
	
	private String tsvFileDir;
	
	//id of the current
	private String currentOntologyBeingProcessed;
	
	private List<OWLGraphWrapper> graphs;

	private List<GafDocument> gafDocuments;
	
	private String dbType;
	
	private Set<AnnotationRuleViolation> annotationRuleViolations;
	
	public DbOperationsTask(String op, String []locations, boolean force, String tablePrefix, String tsvFilesDir){
		this("gold", op, locations, force, tablePrefix, tsvFilesDir);
	}
	
	
	public DbOperationsTask(String dbType, String op, String []locations, boolean force, String tablePrefix, String tsvFilesDir){
		super();
		
		this.dbType = dbType;
		this.opName =op;
		this.locations = locations;
		this.force = force;
		this.tsvFileDir = tsvFilesDir;
		graphs = new Vector<OWLGraphWrapper>();
		gafDocuments = new ArrayList<GafDocument>();
		
		annotationRuleViolations = new HashSet<AnnotationRuleViolation>();
		
		if("checkconsistency".equals(opName) || "find-inferences".equals(opName))		
			this.consitancyCheckBuffer = new StringBuffer();
		
	}
	
	public DbOperationsTask(String op) throws OWLOntologyCreationException, IOException{
		//this(op, new DbOperations().buildOWLGraphWrapper(), false, "", "");
		this(op, null, false, "", "");

	}
	@Override
	public void run(){
		LOG.info("Running Db operation : " + opName);
	
		
		this.exception = null; 
		running = true;
		DbOperationsInterface db = null;
		OWLGraphWrapper graph = null;
		if("gaf".equals(this.dbType)){
			
			DbOperationsService dbservice =(DbOperationsService) ServicesConfig.getService("db-operations");
			if(dbservice != null){
				graph = dbservice.getOntologyGraph();
			}
			db = new GAFDbOperations();
			
			
		}else
			db = new DbOperations();
		
		db.addDbOperationsListener(this);
		
		try{
			
			for(String location: locations){
				this.currentOntologyBeingProcessed = location;
				if("bulkload".equals(opName)){
						db.bulkLoad(location, force);
						if(db instanceof GAFDbOperations && graph != null){
							for(GafDocument doc: gafDocuments){
								
								annotationRuleViolations.addAll(doc.validateAnnotations(graph));
								/*for(AnnotationRuleViolation arv: doc.validateAnnotations(graph)){
									this.annotationRuleViolations.append(arv.getMessage() + "----" + arv.getSourceAnnotation());
								}*/
							}
						}
				}else if ("update".equals(opName)){
						db.updateGold(location);
				}else if ("buildschema".equals(opName)){
						db.buildSchema(force, tablePrefix);
				}else if ("buildtsv".equals(opName)){
					db.dumpFiles(tablePrefix, location);
				}else if ("loadtsv".equals(opName)){
					db.loadTsvFiles(tsvFileDir);
				}else if("checkconsistency".equals(opName)){
					performConsistancyCheck(((DbOperations) db).buildOWLGraphWrapper(location).getSourceOntology());
				}else if("find-inferences".equals(opName)){
					//performConsistancyCheck(db.buildOWLGraphWrapper(location).getSourceOntology());
					findInferences(((DbOperations)db).buildOWLGraphWrapper(location));
				}
			}
		} catch (Exception e) {
			running = false;
			this.exception = e;
			e.printStackTrace();
			LOG.error("DB Operation failed " + opName, e);
		}finally{
			running = false;
		}

	}
	
	public String getOperationName(){
		return opName;
	}
	
	protected void reportStartTime(String name){
		this.addInProgress(name);
	}
	
	protected void reportEndTime(String name){
		this.addCompleted(name);
	}
	
	public void bulkLoadStart() {
		reportStartTime("BulkLoad/TotalTime--"+currentOntologyBeingProcessed);
	}

	public void bulkLoadEnd() {
		reportEndTime("BulkLoad/TotalTime--"+currentOntologyBeingProcessed);
	}

	public void dumpFilesStart() {
		reportStartTime("DumpFiles--"+currentOntologyBeingProcessed);
	}

	public void dumpFilesEnd() {
		reportEndTime("DumpFiles--"+currentOntologyBeingProcessed);

	}

	public void buildSchemaStart() {
		reportStartTime("BuildSchema--"+currentOntologyBeingProcessed);
	}

	public void buildSchemaEnd() {
		reportEndTime("BuildSchema--"+currentOntologyBeingProcessed);
	}

	public void loadTsvFilesStart() {
		reportStartTime("LoadTsvFiles--"+currentOntologyBeingProcessed);
	}

	public void loadTsvFilesEnd() {
		reportEndTime("LoadTsvFiles--"+currentOntologyBeingProcessed);
	}

	public void updateStart() {
		reportStartTime("Update/TotalTime--"+currentOntologyBeingProcessed);
	}

	public void updateEnd() {
		reportEndTime("Update/TotalTime--"+currentOntologyBeingProcessed);
	}


	public void startOntologyLoad() {
		reportStartTime("Obo To OWL Conversion--"+currentOntologyBeingProcessed);
	}


//	public void endOntologyLoad(OWLGraphWrapper graph) {
	public void endOntologyLoad(Object object) {
		reportEndTime("Obo To OWL Conversion--"+currentOntologyBeingProcessed);
		
		if(object instanceof OWLGraphWrapper)
			graphs.add((OWLGraphWrapper)object);
		else if(object instanceof GafObjectsBuilder){
			GafObjectsBuilder builder = (GafObjectsBuilder) object;
			gafDocuments.add( builder.getGafDocument());
			
			this.annotationRuleViolations.addAll(builder.getParser().getAnnotationRuleViolations());
			
			for(String s: builder.getParser().getErrors()){
				this.annotationRuleViolations.add(new AnnotationRuleViolation(s));
			}
			
		}
	}
	
	public List<OWLGraphWrapper> getGraphs(){
		return graphs;
	}
	
	public String getConsistencyCheckResults(){
		return consitancyCheckBuffer == null ? null : consitancyCheckBuffer.toString();
	}
	
	public Set<AnnotationRuleViolation> getAnnotationVoilations(){
		return this.annotationRuleViolations;
	}
	
	private StringBuffer consitancyCheckBuffer;
	
	private void performConsistancyCheck(OWLOntology ontology){
		
		PelletReasonerFactory factory = new PelletReasonerFactory();
		OWLReasoner reasoner = factory.createReasoner(ontology);
		
		consitancyCheckBuffer.append("<h2>Performing Consitancy Check for ontology :" + Owl2Obo.getIdentifier(ontology.getOntologyID().getOntologyIRI()) + "</h2>");
		
		boolean isConsistent= reasoner.isConsistent();
		
		consitancyCheckBuffer.append("is Consistent: " + isConsistent + "<br />");
		
		Node<OWLClass> unsatisfiableClasses = reasoner.getUnsatisfiableClasses();
		if (unsatisfiableClasses.getSize() > 0) {
			consitancyCheckBuffer.append("<br /><br /><h3>The following classes are unsatisfiable: </h3><br />");
			for(OWLClass cls : unsatisfiableClasses) {
				if (cls.toString().endsWith("Nothing"))
					continue;
				consitancyCheckBuffer.append ("&nbsp;&nbsp;&nbsp;-unsatisfiable: " + Owl2Obo.getIdentifier(cls) + "<br />");
			}
		}
		else {
			consitancyCheckBuffer.append("There are no unsatisfiable classes<br />");
		}
		
	}

	private void findInferences(OWLGraphWrapper graph){
		OWLOntology ontology = graph.getSourceOntology();
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
				
				if(cls.toString().equals(ec.toString()))
					continue;
				
				if (cls.toString().compareTo(ec.toString()) > 0) // equivalence is symmetric: report each pair once
					consitancyCheckBuffer.append("&nbsp;&nbsp;&nbsp;* INFERRED: equivalent "+getLabel(cls, graph)+" "+ getLabel(ec, graph) + "<br />" );
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
					for (OWLClassExpression ec : cls.getEquivalentClasses(ontology)) {
						
						if (ec instanceof OWLObjectIntersectionOf) {
							OWLObjectIntersectionOf io = (OWLObjectIntersectionOf)ec;
							for (OWLClassExpression op : io.getOperands()) {
								if (op.equals(sc)) {
									isAsserted = true;
								}
							}
						}
					}
					if (!isAsserted) {
						this.consitancyCheckBuffer.append("&nbsp;&nbsp;&nbsp;&nbsp;*INFERRED:  "+getLabel(cls,graph)+" subClassOf "+getLabel(sc, graph) + "<br />");
					}
				}
			}
		}
		
		
	}
	
	private static String getLabel(OWLClass cls,  OWLGraphWrapper graph) {
		return  "["+cls.toString()+" ! "+ graph.getLabel(cls) +"]";
	}

	
	
}
