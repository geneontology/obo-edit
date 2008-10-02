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

public class SubPropertyRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SubPropertyRule.class);

	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		if (reasoner.getProperties() == null) {
			logger.error("no properties to make new inferences. Reasoner not initialized?");
			return null;
		}
		for (OBOProperty subProp : reasoner.getProperties()) {
			Collection<LinkedObject> superProps = reasoner.getParentsOfType(subProp, OBOProperty.IS_A);
			if (superProps.size() > 0) {
				Collection<Link> links = reasoner.getLinks(subProp);
				for (Link link : links) {
					for (LinkedObject superProp : superProps) {
						Link out = createLink(link.getChild(), (OBOProperty) superProp, link.getParent());
						AbstractExplanation exp;
						exp = new LinkCompositionExplanation(link, null); // TODO
						exp.setExplainedLink(out);
						expls.add(exp);
					}
				}
			}
		}
		return expls;	
	}


}
