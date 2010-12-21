package owltools.sim;

import org.semanticweb.owlapi.model.OWLObject;

public class JaccardSimilarity extends Similarity {

	@Override
	public void calculate(SimEngine simEngine, OWLObject a, OWLObject b) {
		double ci = simEngine.getCommonSubsumersSize(a, b);
		double cu = simEngine.getUnionSubsumersSize(a, b);
		setScore( ci / cu );
	}

}
