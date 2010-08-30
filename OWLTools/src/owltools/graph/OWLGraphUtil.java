package owltools.graph;

import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLNaryBooleanClassExpression;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.model.OWLPropertyExpression;
import org.semanticweb.owlapi.model.OWLRestriction;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

public class OWLGraphUtil {

	OWLOntology ontology;
	
	public OWLGraphUtil(OWLOntology ontology) {
		super();
		this.ontology = ontology;
	}

	/**
	 * this method is for retrieving edges between _named_ objects
	 * 
	 * e.g. (A SubClassOf R some B) => <A, R-some, B>
	 * @param source
	 * @return all edges that originate from source
	 */
	public  Set<OWLGraphEdge> getOutgoingEdges(OWLObject cls) {
		Set<OWLGraphEdge> pEdges = getPrimitiveOutgoingEdges(cls);
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		for (OWLGraphEdge e : pEdges) {
			edges.addAll(primitiveEdgeToFullEdges(e));
		}
		return edges;
	}

	private Set<OWLGraphEdge> primitiveEdgeToFullEdges(OWLGraphEdge e) {
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		if (e.isTargetNamedObject()) {
			edges.add(e); // do nothing
		}
		else {
			edges = getOutgoingEdges(e.getTarget());
		}
		return edges;
	}

	/**
	 * e.g. (A SubClassOf R some B) => <A,sub,R-some-B>, <R-some-B,R-some,B>
	 * @param source
	 * @return
	 */
	public  Set<OWLGraphEdge> getPrimitiveOutgoingEdges(OWLObject s) {
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		if (s instanceof OWLClass) {
			for (OWLSubClassOfAxiom sca : ontology.getSubClassAxiomsForSubClass((OWLClass) s)) {
				edges.add(axiomToPrimitiveEdge(sca));
			}
			for (OWLEquivalentClassesAxiom eqa : ontology.getEquivalentClassesAxioms((OWLClass) s)) {
				edges.add(axiomToPrimitiveEdge(eqa));
			}

		}
		else if (s instanceof OWLRestriction<?>) {
			edges.add(restrictionToPrimitiveEdge((OWLRestriction<?>) s));
		}
		else if (s instanceof OWLObjectIntersectionOf) {
			for (OWLClassExpression ce : ((OWLObjectIntersectionOf)s).getOperands()) {
				edges.add(expressionToPrimitiveEdge(s,ce));
			}
		}
		else if (s instanceof OWLObjectUnionOf) {
			// do nothing in this direction
		}
		return edges;
	}

	/**
	 * e.g. R-some-B ==> <R-some-B,R,B>
	 */
	private OWLGraphEdge restrictionToPrimitiveEdge(OWLRestriction s) {
		OWLObjectPropertyExpression p = null;
		OWLObject t = null;
		OWLGraphEdgeLabel.Quantifier q = null;
		if (s instanceof OWLObjectSomeValuesFrom) {
			t  = ((OWLObjectSomeValuesFrom)t).getFiller();
			p = (OWLObjectPropertyExpression) s.getProperty();
		}
		return new OWLGraphEdge(s,t,p,q,ontology);
	}

	public OWLGraphEdge axiomToPrimitiveEdge(OWLAxiom a) {

		if (a instanceof OWLSubClassOfAxiom) {
			OWLSubClassOfAxiom x = (OWLSubClassOfAxiom)a;
			return expressionToPrimitiveEdge(x.getSubClass(), x.getSuperClass());
		}
		throw new Error("cannot handle "+a);
	}
	
	public OWLGraphEdge expressionToPrimitiveEdge(OWLObject s, OWLClassExpression t) {
		OWLGraphEdgeLabel el = null;
		return new OWLGraphEdge(s,t,ontology,el);
	}

}
