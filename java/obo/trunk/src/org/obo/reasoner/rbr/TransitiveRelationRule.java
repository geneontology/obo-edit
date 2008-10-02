package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.obo.datamodel.Link;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class TransitiveRelationRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransitiveRelationRule.class);

	protected static Link temp = new OBORestrictionImpl();
	long implicationTime;
	long fetchTime;
	long expConstructTime;

	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		implicationTime = 0;
		fetchTime = 0;
		expConstructTime = 0;
	}
	
	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		for (OBOProperty inferredProp : reasoner.getProperties()) {
			if (inferredProp.isTransitive()) {
				expls.addAll(getNewInferencesForComposition(reasoner, inferredProp, inferredProp, inferredProp));
			}
		}
		// is_a is transitive
		OBOProperty is_a = OBOProperty.IS_A;
		expls.addAll(getNewInferencesForComposition(reasoner, is_a, is_a, is_a));
		return expls;	
	}


	@Override
	public String toString() {
		return "Transitivity rule (implicationTime = "
				+ (implicationTime / 1000000d) + ", expConstTime = "
				+ (expConstructTime / 1000000d) + ", fetchTime = "
				+ (fetchTime / 1000000d) + ")";
	}

}
