package owltools.graph;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLPropertyExpression;
import org.semanticweb.owlapi.model.OWLRestriction;

import owltools.graph.OWLGraphEdgeLabel.Quantifier;

public class OWLGraphEdge {
	
	
	private OWLObject source;
	private OWLObject target;
	private OWLOntology ontology;
	OWLGraphEdgeLabel edgeLabel;
	

	
	public OWLGraphEdge(OWLObject source, OWLObject target,
			OWLOntology ontology, OWLGraphEdgeLabel edgeLabel) {
		super();
		this.source = source;
		this.target = target;
		this.ontology = ontology;
		this.edgeLabel = edgeLabel;
	}

	public OWLGraphEdge(OWLRestriction s, OWLObject t,
			OWLGraphEdgeLabel el, OWLOntology o) {
		super();
		this.source = s;
		this.target = t;
		this.ontology = o;
		this.edgeLabel = el;
	}


	
	public OWLGraphEdge(OWLObject source, OWLObject target, OWLOntology ontology) {
		super();
		this.source = source;
		this.target = target;
		this.ontology = ontology;
	}


	public OWLGraphEdge(OWLObject source, OWLObject target) {
		super();
		this.source = source;
		this.target = target;
	}


	public OWLGraphEdge(OWLRestriction s, OWLObject t, OWLObjectPropertyExpression p,
			Quantifier q, OWLOntology o) {
		super();
		OWLGraphEdgeLabel el = new OWLGraphEdgeLabel(p,q);
		this.source = s;
		this.target = t;
		this.ontology = o;
		this.edgeLabel = el;
	}


	public OWLObject getSource() {
		return source;
	}
	public void setSource(OWLObject source) {
		this.source = source;
	}
	public OWLObject getTarget() {
		return target;
	}
	public void setTarget(OWLObject target) {
		this.target = target;
	}
	
	public OWLGraphEdgeLabel getEdgeLabel() {
		return edgeLabel;
	}
	public void setEdgeLabel(OWLGraphEdgeLabel edgeLabel) {
		this.edgeLabel = edgeLabel;
	}
	public OWLOntology getOntology() {
		return ontology;
	}
	public void setOntology(OWLOntology ontology) {
		this.ontology = ontology;
	}


	public boolean isSourceNamedObject() {
		return (source instanceof OWLNamedObject);
	}
	public boolean isTargetNamedObject() {
		return (target instanceof OWLNamedObject);
	}

	public String toString() {
		return "<"+source+" "+edgeLabel+" "+target+">";
	}

}
