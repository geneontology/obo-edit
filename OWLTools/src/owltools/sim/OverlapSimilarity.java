package owltools.sim;

import org.semanticweb.owlapi.model.OWLObject;

public class OverlapSimilarity extends Similarity {

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) {
		setScore(simEngine.getCommonSubsumersSize(a, b));

	}

}
