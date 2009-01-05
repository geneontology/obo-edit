package org.obo.datamodel;

import java.util.*;

import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.util.TermUtil;
import org.apache.log4j.*;

public interface RootAlgorithm {
	//initialize logger
	final Logger logger = Logger.getLogger(RootAlgorithm.class);
	/**
	 * This algorithm will only return roots that truly have no parents
	 */
	public static final RootAlgorithm STRICT = new AbstractRootAlgorithm() {
		public boolean isRoot(LinkedObject lo) {
			if (lo instanceof Instance)
				return true;

			Iterator it = lo.getParents().iterator();

			while (it.hasNext()) {
				Relationship link = (Relationship) it.next();
				if (link instanceof Link
						&& ((Link) link).getParent() instanceof DanglingObject)
					continue;

				if (link instanceof ValueLink
						&& !(((ValueLink) link).getValue() instanceof IdentifiedObject)) {
					continue;
				}

				if (link instanceof Link)
					if (TermUtil.isObsolete(((Link) link).getParent())
							|| TermUtil.isObsolete(((Link) link).getType()))
						continue;

				return false;
			}

			return GREEDY.isRoot(lo);
		}

		public String toString() {
			return "STRICT";
		}
	};

	public static final RootAlgorithm GREEDY = new AbstractRootAlgorithm() {
		public boolean isRoot(LinkedObject lo) {
//			logger.debug("RootAlgorithm isRoot -- checking: " + lo);		

			if (lo instanceof Instance)
				return true;
			Collection parents = linkDatabase.getParents(lo);
			Iterator it = parents.iterator();

			while (it.hasNext()) {
				Relationship link = (Relationship) it.next();
//				logger.debug("link: " + link);
				if (link instanceof Link
						&& ((Link) link).getParent() instanceof DanglingObject)
					continue;

				if (link instanceof Link)
					if (TermUtil.isObsolete(((Link) link).getParent())
							|| TermUtil.isObsolete(((Link) link).getType()))
						continue;

				if (link instanceof ValueLink && !(((ValueLink) link).getValue() instanceof IdentifiedObject)) {
					continue;
				}
				
				if (link.getType().isTransitive()){
					return false;
				}
			}
			return true;
		}

		public String toString() {
			return "GREEDY";
		}
	};

	public static abstract class AbstractRootAlgorithm implements RootAlgorithm {
		protected LinkDatabase linkDatabase = DefaultLinkDatabase.getDefault();
		protected Iterator sourceSet;

		public void setLinkDatabase(LinkDatabase linkDatabase) {
			this.linkDatabase = linkDatabase;
		}

		public void setSources(Iterator sourceSet) {
			this.sourceSet = sourceSet;
		}
	}

	public boolean isRoot(LinkedObject lo);

	public void setLinkDatabase(LinkDatabase linkDatabase);
}
