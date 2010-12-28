package owltools.graph;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLPropertyExpression;
import org.semanticweb.owlapi.model.OWLRestriction;

import owltools.graph.OWLQuantifiedProperty.Quantifier;

public class OWLGraphEdge {
	
	
	private OWLObject source;
	private OWLObject target;
	private OWLOntology ontology;
	private int distance = 1;
	private List<OWLQuantifiedProperty> quantifiedPropertyList = new Vector<OWLQuantifiedProperty>();
	
	public OWLGraphEdge(OWLObject source, OWLObject target,
			OWLOntology ontology, OWLQuantifiedProperty qp) {
		super();
		this.source = source;
		this.target = target;
		this.ontology = ontology;
		setSingleQuantifiedProperty(qp);
	}

	public OWLGraphEdge(OWLRestriction s, OWLObject t,
			OWLQuantifiedProperty el, OWLOntology o) {
		super();
		this.source = s;
		this.target = t;
		this.ontology = o;
		setSingleQuantifiedProperty(el);
	}

	public OWLGraphEdge(OWLObject source, OWLObject target, List<OWLQuantifiedProperty> qpl, OWLOntology ontology) {
		super();
		this.source = source;
		this.target = target;
		this.ontology = ontology;
		this.quantifiedPropertyList = qpl;
	}

	
	public OWLGraphEdge(OWLObject source, OWLObject target, OWLOntology ontology) {
		super();
		this.source = source;
		this.target = target;
		this.ontology = ontology;
		setSingleQuantifiedProperty(new OWLQuantifiedProperty()); // defaults to subclass
	}


	public OWLGraphEdge(OWLObject source, OWLObject target) {
		super();
		this.source = source;
		this.target = target;
	}


	public OWLGraphEdge(OWLObject s, OWLObject t, OWLObjectPropertyExpression p,
			Quantifier q, OWLOntology o) {
		super();
		OWLQuantifiedProperty el = new OWLQuantifiedProperty(p,q);
		this.source = s;
		this.target = t;
		this.ontology = o;
		setSingleQuantifiedProperty(el);
	}


	public OWLGraphEdge(OWLObject s, OWLObject t, Quantifier q) {
		super();
		OWLQuantifiedProperty el = new OWLQuantifiedProperty(null,q);
		this.source = s;
		this.target = t;
		setSingleQuantifiedProperty(el);
	}

	public OWLObject getSource() {
		return source;
	}
	public String getSourceId() {
		return source.toString();
	}
	public void setSource(OWLObject source) {
		this.source = source;
	}
	public OWLObject getTarget() {
		return target;
	}
	public String getTargetId() {
		return target.toString();
	}

	public void setTarget(OWLObject target) {
		this.target = target;
	}
	
	public int getDistance() {
		return distance;
	}

	public void setDistance(int distance) {
		this.distance = distance;
	}


	public List<OWLQuantifiedProperty> getQuantifiedPropertyList() {
		return quantifiedPropertyList;
	}

	public void setQuantifiedPropertyList(List<OWLQuantifiedProperty> qps) {
		this.quantifiedPropertyList = qps;
	}

	public OWLQuantifiedProperty getSingleQuantifiedProperty() {
		return quantifiedPropertyList.iterator().next();
	}
	
	public OWLQuantifiedProperty getLastQuantifiedProperty() {
		/*OWLQuantifiedProperty lqp = null;
		for (OWLQuantifiedProperty qp : quantifiedPropertyList) {
			lqp = qp;
		}
		return lqp;*/
		
		return (OWLQuantifiedProperty)quantifiedPropertyList.get(quantifiedPropertyList.size()-1);
	}

	public void setSingleQuantifiedProperty(OWLQuantifiedProperty qp) {
		quantifiedPropertyList = new Vector<OWLQuantifiedProperty>();
		quantifiedPropertyList.add(qp);
	}

	public OWLQuantifiedProperty getFinalQuantifiedProperty() {
		return quantifiedPropertyList.get(quantifiedPropertyList.size()-1);
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
		return "["+source+" "+getQuantifiedPropertyList()+" "+target+"]";
	}
	
	public int hashCode() {
		return toString().hashCode();
	}
	
	public boolean equals(Object e) {
		
		if(e == null && !(e instanceof OWLGraphEdge))
			return false;
		
		OWLGraphEdge other = (OWLGraphEdge) e;
		
		return other.getSourceId().equals(getSourceId()) &&
			other.getTargetId().equals(getTargetId())
			&& quantifiedPropertyList.containsAll(other.getQuantifiedPropertyList());
		
	}


}
