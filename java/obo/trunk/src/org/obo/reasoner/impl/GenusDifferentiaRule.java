package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class GenusDifferentiaRule extends AbstractReasonerRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GenusDifferentiaRule.class);

	public GenusDifferentiaRule() {
		setAllowIntersections(true);
	}
	
	@Override
	protected Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		if (!TermUtil.isIntersection(newLink))
			return null;
		Link out = createLink(newLink.getChild(), newLink
				.getType(), newLink.getParent());
		AbstractExplanation exp;
		if (reasoner.isSubPropertyOf(out.getType(), OBOProperty.IS_A))
			exp = new GenusExplanation(newLink);
		else
			exp = new DifferentiaExplanation(newLink);
		exp.setExplainedLink(out);
		ArrayList<Explanation> c = new ArrayList<Explanation>(1);
		c.add(exp);
		return c;
	}

}
