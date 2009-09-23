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
			for(Object parent : lo.getParents()){
				Relationship link = (Relationship) parent;
				if (link instanceof Link
						&& ((Link) link).getParent() instanceof DanglingObject)
					continue;

				if (link instanceof ValueLink && !(((ValueLink) link).getValue() instanceof IdentifiedObject)) {
					continue;
				}

				if (link instanceof Link)
					if (TermUtil.isObsolete(((Link) link).getParent()) || TermUtil.isObsolete(((Link) link).getType()))
						continue;
				
				if(link.getType().equals(OBOProperty.DISJOINT_FROM)){
					continue;
				}
				if(link.getType().equals(OBOProperty.UNION_OF)){
					continue;
				}

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
			
			if (lo instanceof Instance)
				return true;
				
			for(Object parent : linkDatabase.getParents(lo)){
				Relationship parent_link = (Relationship) parent;
				
				//check for inverse relations when the LinkedObject does not have any parents 
				// terms with inverse relations - part_of has_part relations might not be displayed as roots 
				// ex: X --has_part--> Z INVERSE_OF Z --part_of --> X
				for(Object child : linkDatabase.getChildren(lo)){
					Relationship child_link = (Relationship) child;
					
					
					if(parent_link.getType().getName().equals(OBOProperty.HAS_PART.getName())){
		
						if(child_link.getType().getName().equals(OBOProperty.PART_OF.getName())){			
							return true;
						}
					}
					
				}
				
				if (parent_link instanceof Link && ((Link) parent_link).getParent() instanceof DanglingObject)
					continue;

				if (parent_link instanceof Link)
					if (TermUtil.isObsolete(((Link) parent_link).getParent())
							|| TermUtil.isObsolete(((Link) parent_link).getType()))
						continue;

				if (parent_link instanceof ValueLink && !(((ValueLink) parent_link).getValue() instanceof IdentifiedObject)) {
					continue;
				}

				if(parent_link.getType().equals(OBOProperty.DISJOINT_FROM)){
					continue;
				}
				if(parent_link.getType().equals(OBOProperty.UNION_OF)){
					continue;
				}
				return false;
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
