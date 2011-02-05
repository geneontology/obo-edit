package owltools.sim;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

public class JaccardSimilarity extends Similarity {

	public JaccardSimilarity() {
		super();
		minScore = 0.2; // default
	}

	
	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) {
		double ci = simEngine.getCommonSubsumersSize(a, b);
		double cu = simEngine.getUnionSubsumersSize(a, b);
		setScore( ci / cu );
	}

	@Override
	protected void translateResultsToOWLAxioms(String id,
			OWLNamedIndividual result, Set<OWLAxiom> axioms) {
		// TODO Auto-generated method stub
		
	}

}
