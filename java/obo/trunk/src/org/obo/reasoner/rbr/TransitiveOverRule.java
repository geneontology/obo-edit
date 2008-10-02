package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class TransitiveOverRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransitiveOverRule.class);

	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		for (OBOProperty inferredProp : reasoner.getProperties()) {
			OBOProperty op = inferredProp.getTransitiveOver();
			if (op != null) {
				expls.addAll(getNewInferencesForComposition(reasoner, inferredProp, inferredProp, op));
			}
		}
		return expls;	
	}


}
