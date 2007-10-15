package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public class GenusDifferentiaRule extends AbstractReasonerRule {

	public GenusDifferentiaRule() {
		setAllowIntersections(true);
	}
	
	@Override
	protected Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		if (!TermUtil.isIntersection(newLink))
			return null;
		OBORestriction out = new OBORestrictionImpl(newLink.getChild(), newLink
				.getType(), newLink.getParent(), true);
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
