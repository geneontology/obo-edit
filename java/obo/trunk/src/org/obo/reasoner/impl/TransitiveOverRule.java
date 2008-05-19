package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class TransitiveOverRule extends AbstractReasonerRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransitiveOverRule.class);

	public TransitiveOverRule() {
		setAllowIntersections(true);
	}
	
	@Override
	protected Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		OBOProperty overProp =  newLink.getType().getTransitiveOver();
		if (overProp == null)
			return null;
		ArrayList<Explanation> c = new ArrayList<Explanation>();
		for (LinkedObject parent : 
			reasoner.getParentsOfType(newLink.getParent(), overProp)) {
			
			Link out = createLink(newLink.getChild(), newLink
				.getType(), parent);
			AbstractExplanation exp;
			exp = new TransitiveOverExplanation(newLink);
			exp.setExplainedLink(out);
			c.add(exp);
		}
		return c;
	}

}
