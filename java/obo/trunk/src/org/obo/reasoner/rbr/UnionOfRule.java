package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class UnionOfRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UnionOfRule.class);

	private ArrayList<Explanation> expls;
	
	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		expls = new ArrayList<Explanation>();
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				for (Link link : lo.getParents()) {
					if (TermUtil.isUnion(link)) {
						Link out = createLink(link.getParent(), 
								OBOProperty.IS_A, 
								link.getChild());
						AbstractExplanation exp;
						exp = new LinkCompositionExplanation(link, null); // TODO
						exp.setExplainedLink(out);	
						expls.add(exp);
					}
				}
			}
		}
	}

	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		ArrayList<Explanation> newExpls = expls;
		if (expls.size() > 0)
			expls = new ArrayList<Explanation>(); // only return once
		// there is no rule as such here - all is_a links materialized at initialization step
		return newExpls;	
	}


}
