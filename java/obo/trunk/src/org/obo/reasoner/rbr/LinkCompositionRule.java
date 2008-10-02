package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

public class LinkCompositionRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkCompositionRule.class);

	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		logger.debug("all properties: "+reasoner.getProperties());
		for (OBOProperty inferredProp : reasoner.getProperties()) {
			Collection<List<OBOProperty>> chains = inferredProp.getHoldsOverChains();
			if (chains == null)
				continue;
			for (List<OBOProperty> chain : chains) {
				if (chain.size() == 2) {
					expls.addAll(getNewInferencesForComposition(reasoner, inferredProp, chain.get(0), chain.get(1)));
				}
				else {
					// TODO
				}
			}
		}
		return expls;	
	}



}
