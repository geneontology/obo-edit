package org.obo.filters;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class MultipleRootSearchCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultipleRootSearchCriterion.class);

	public static final MultipleRootSearchCriterion CRITERION = new MultipleRootSearchCriterion();

	public boolean matches(IdentifiedObject o) {
		if (o instanceof LinkedObject) {
			Collection<LinkedObject> parents;
			if (reasoner != null) {
				Collection<Link> parentLinks = reasoner
						.getParents((LinkedObject) o);
				RootAlgorithm.GREEDY.setLinkDatabase(reasoner);
				int rootCount = 0;
				HashSet<LinkedObject> parentSet = new LinkedHashSet<LinkedObject>();
				for (Link link : parentLinks) {
					parentSet.add(link.getParent());
				}
				for (LinkedObject obj : parentSet) {
					if (RootAlgorithm.GREEDY.isRoot(obj)) {
						rootCount++;
					}
					if (rootCount > 1)
						return true;

				}
			} else {
				parents = TermUtil.getAncestors((LinkedObject) o);
				int rootCount = 0;
				LinkDatabase linkDB = new DefaultLinkDatabase(null);
				RootAlgorithm.GREEDY.setLinkDatabase(linkDB);
				for (LinkedObject lo : parents) {
					if (RootAlgorithm.GREEDY.isRoot(lo)) {
						rootCount++;
					}
					if (rootCount > 1)
						return true;
				}
			}
		}
		return false;
	}

	public String getID() {
		return "multiple_roots";
	}

	public String toString() {
		return "Has Multiple Roots";
	}

}
