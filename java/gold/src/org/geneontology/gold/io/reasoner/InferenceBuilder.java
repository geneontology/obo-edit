package org.geneontology.gold.io.reasoner;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.geneontology.web.TaskExecution;
import org.geneontology.web.TaskExecutionListener;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import com.clarkparsia.owlapi.explanation.DefaultExplanationGenerator;
import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty.Quantifier;

public class InferenceBuilder implements TaskExecution {

	
	
	private OWLReasoner reasoner;
	
	//publ PelletReasonerFactory factory;
	
	private OWLGraphWrapper graph;
	
	//private OWLOntology infOntology;
	
	private List<TaskExecutionListener> listeners;
	
	
	public OWLGraphWrapper getOWLGraphWrapper(){
		return this.graph;
	}

	public void setOWLGraphWrapper(OWLGraphWrapper g){
		this.reasoner = null;
		this.graph =g;
	}
	
	
	public InferenceBuilder(OWLGraphWrapper graph){
		this.graph = graph;
		listeners =new ArrayList<TaskExecutionListener>();
	}
	

	
	private void buildDuplicate(){
		/*try{
			infOntology = graph.getManager().createOntology(graph.getSourceOntology().getOntologyID());
			OWLOntologyManager manager = graph.getManager();
			for(OWLAxiom ax: graph.getSourceOntology().getAxioms()){
				manager.applyChange(new AddAxiom(infOntology, ax));
			}
		}catch(Exception ex){
			ex.printStackTrace();
		}*/
		
	}
	
	public List<OWLGraphEdge> buildInferences() {
		List<OWLGraphEdge> sedges = new ArrayList<OWLGraphEdge>();

		List<OWLGraphEdge> eedges = new ArrayList<OWLGraphEdge>();

		
	//	buildDuplicate();
		
	//	OWLDataFactory dataFactory = graph.getDataFactory();
		
	//	ReasonerFactory factory = new ReasonerFactory();
		
	//	reasoner = factory
		//		.createDefaultOwlReasoner(ontology);

		OWLOntology ontology = graph.getSourceOntology();
		
		if(reasoner == null){
			PelletReasonerFactory factory = new PelletReasonerFactory();
			reasoner = factory.createReasoner(ontology);
		}
		
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

					eedges.add(new OWLGraphEdge(cls, ec, Quantifier.EQUIVALENT));
				
		//			graph.getManager().applyChange(new AddAxiom(infOntology, dataFactory.getOWLEquivalentClassesAxiom(cls, ec)  ));
				/*
				 * reasonerComputationsResults
				 * .append("&nbsp;&nbsp;&nbsp;* INFERRED: equivalent " +
				 * getLabel(cls, graph) + " " + getLabel(ec, graph) + "<br />");
				 */
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
						sedges.add(new OWLGraphEdge(cls, sc,
								Quantifier.SUBCLASS_OF));
						
			//			graph.getManager().applyChange(new AddAxiom(infOntology, dataFactory.getOWLSubClassOfAxiom(cls, sc)  ));
				
						/*
						 * this.reasonerComputationsResults
						 * .append("&nbsp;&nbsp;&nbsp;&nbsp;*INFERRED:  " +
						 * getLabel(cls, graph) + " subClassOf " + getLabel(sc,
						 * graph) + "<br />");
						 */
					}
				}
			}
		}

		
	//	reasoner = factory.createReasoner(infOntology);
		sedges.addAll(eedges);
		return sedges;

	}
	
	public Set<Set<OWLAxiom>> getExplaination(String c1, String c2, Quantifier quantifier){
		/*OWLAxiom ax = null;
		OWLDataFactory dataFactory = graph.getDataFactory();
		OWLClass cls1 = dataFactory.getOWLClass(IRI.create(c1));
		OWLClass cls2 = dataFactory.getOWLClass(IRI.create(c2));
		
		if(quantifier == Quantifier.EQUIVALENT){
			ax = dataFactory.getOWLEquivalentClassesAxiom(cls1, cls2);
		}else{
			ax = dataFactory.getOWLSubClassOfAxiom(cls1, cls2);
		}
		
		
		//graph.getManager().applyChange(new AddAxiom(graph.getSourceOntology(), ax));
		
		DefaultExplanationGenerator gen = new DefaultExplanationGenerator(graph.getManager(), factory, infOntology, 
				reasoner,null);
		
	
		return gen.getExplanations(ax);*/
		
		return null;
	}

	private List<OWLGraphEdge> edges;
	
	@Override
	public void execute() {
		edges = buildInferences();
		fireTaskExecutionListener(edges);
	}

	@Override
	public Object getData() {
		return edges;
	}
	
	protected void fireTaskExecutionListener(List<OWLGraphEdge> edges){
		for(TaskExecutionListener l: listeners){
			l.updateData(edges);
		}
	}

	@Override
	public void addTaskExecutionListener(TaskExecutionListener listener) {
		listeners.add(listener);
	}

}
