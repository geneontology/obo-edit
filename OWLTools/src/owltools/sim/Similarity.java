package owltools.sim;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLPropertyRange;
import org.semanticweb.owlapi.model.OWLQuantifiedRestriction;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.sim.SimEngine.SimilarityAlgorithmException;

/**
 * Represents a pairwise similarity between two OWLObjects.
 * 
 * todo: consider making a and b immuntable
 *  
 * @author cjm
 *
 */
public abstract class Similarity {
	OWLObject a;
	OWLObject b;
	Double score;
	SimEngine simEngine;

	public Similarity() {
		super();
	}

	public Double getScore() {
		return score;
	}
	public void setScore(Double score) {
		this.score = score;
	}
	public void setScore(int score) {
		this.score = (double) score;
	}


	public String toString() {
		return "S:"+score;
	}
	public void print() {
		print(System.out);
	}
	public void print(PrintStream s) {
		s.println(toString());
	}
	public void print(PrintStream s, OWLObject x) {
		String label = simEngine.getGraph().getLabel(x);
		if (label == null)
			s.print(x.toString());
		else
			s.print(x.toString()+" \""+label+"\"");
	}


	public void printDescription(PrintStream s, OWLObject x) {
		printDescription(s,x,0);
	}
	public void printDescription(PrintStream s, OWLObject x, int depth) {
		depth++;
		OWLGraphWrapper g = simEngine.getGraph();
		if (x instanceof OWLNamedIndividual) {
			for (OWLGraphEdge e : g.getPrimitiveOutgoingEdges(x)) {
				OWLObject c = e.getTarget();
				if (c instanceof OWLClassExpression) {
					printDescription(s, c, depth);
				}
			}
		}
		else if (x instanceof OWLObjectIntersectionOf) {
			int n = 0;
			for (OWLClassExpression y : ((OWLObjectIntersectionOf)x).getOperands()) {
				if (n>0)
					s.print(" and ");
				printDescription(s, y, depth);
				n++;
			}
		}
		else if (x instanceof OWLObjectUnionOf) {
			int n = 0;
			if (depth>0)
				s.print("(");
			for (OWLClassExpression y : ((OWLObjectUnionOf)x).getOperands()) {
				if (n>0)
					s.print(" or ");
				printDescription(s, y, depth);
				n++;
			}
			if (depth>0)
				s.print(")");
		}
		else if (x instanceof OWLQuantifiedRestriction) {
			OWLPropertyRange y = ((OWLQuantifiedRestriction)x).getFiller();
			if (depth>0)
				s.print("(");
			printDescription(s,((OWLQuantifiedRestriction)x).getProperty());
			s.print(" ");
			if (x instanceof OWLObjectAllValuesFrom) {
				s.print("only");
			}
			else if (x instanceof OWLObjectSomeValuesFrom) {
				s.print("some");
			}
			s.print(" ");
			printDescription(s, y, depth);
			if (depth>0)
				s.print(")");
		}
		else if (x instanceof OWLNamedObject) {

			String label = simEngine.getGraph().getLabel(x);

			if (label == null) {
				label = ((OWLNamedObject)x).getIRI().getFragment();
			}
			if (label == null) {
				label = ((OWLNamedObject)x).getIRI().toString();
			}
			s.print(label);

			if (x instanceof OWLClass && label.contains("Phenotype Class")) {
				// TODO - make this configurable!
				for (OWLOntology ont : g.getAllOntologies()) {
					for (OWLEquivalentClassesAxiom ax : ont.getEquivalentClassesAxioms((OWLClass)x)) {
						for (OWLClassExpression y : ax.getClassExpressions()) {
							if (y instanceof OWLClass)
								continue;
							s.print("==");
							printDescription(s, y,depth);
						}
					}
				}
			}

		}
		else {
			s.print(x.toString());
		}
	}

	/**
	 * @param simEngine
	 * @param a
	 * @param b
	 * @throws SimilarityAlgorithmException
	 */
	public abstract void calculate(SimEngine simEngine, OWLObject a, OWLObject b) throws SimilarityAlgorithmException;

	public OWLOntology createOWLOntologyFromResults() throws OWLOntologyCreationException {
		OWLGraphWrapper graph = simEngine.getGraph();
		OWLOntology ont = graph.getManager().createOntology();
		addResultsToOWLOntology(ont);
		return ont;
	}

	public void addResultsToOWLOntology(OWLOntology ont) {
		OWLGraphWrapper graph = simEngine.getGraph();
		for (OWLAxiom axiom: translateResultsToOWLAxioms()) {
			AddAxiom aa = new AddAxiom(ont, axiom);
			graph.getManager().applyChange(aa);
		}
	}

	public Set<OWLAxiom> translateResultsToOWLAxioms() {
		Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		// todo
		return axioms;
	}


}
