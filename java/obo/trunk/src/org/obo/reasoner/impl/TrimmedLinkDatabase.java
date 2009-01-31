package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.bbop.util.Subset;
import org.bbop.util.VectorFilter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.util.ReasonerUtil;

import org.apache.log4j.*;

/**
 * A view over a LinkDatabase in which links are trimmed
 *
 */
public class TrimmedLinkDatabase extends AbstractLinkDatabase  implements LinkDatabase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TrimmedLinkDatabase.class);

	protected LinkDatabase linkDatabase;

	protected boolean enableTrimming = true;

	public static long trimTime = 0;

	protected static class LinkTarget {
		protected OBOProperty type;

		protected LinkedObject parent;

		public LinkTarget() {

		}

		public LinkTarget(Link link) {
			this.type = link.getType();
			this.parent = link.getParent();
		}

		@Override
		public int hashCode() {
			return type.hashCode() + parent.hashCode();
		}

		@Override
		public boolean equals(Object o) {
			return ((LinkTarget) o).parent.equals(parent)
					&& ((LinkTarget) o).type.equals(type);
		}

		public void setType(OBOProperty type) {
			this.type = type;
		}

		public void setParent(LinkedObject parent) {
			this.parent = parent;
		}
	}

	public void setEnableTrimming(boolean enableTrimming) {
		this.enableTrimming = enableTrimming;
	}

	public boolean normalTrimming = true;

	public Collection<Link> getChildren(LinkedObject lo) {
		if (!enableTrimming)
			return linkDatabase.getChildren(lo);

		VectorFilter<Link> filter = new VectorFilter<Link>() {
			public boolean satisfies(Link inLink) {
				boolean trim;
				if (normalTrimming)
					trim = !ReasonerUtil.shouldBeTrimmed(linkDatabase, inLink);
				else
					trim = !ReasonerUtil.shouldBeTrimmedNew(linkDatabase, inLink);
				return trim;
			}
		};

		Collection<Link> children = linkDatabase.getChildren(lo);
		Collection<Link> out = new Subset<Link>(filter, children, false);
		return out;
	}


	public Collection<Link> getParents(LinkedObject lo) {
		if (!enableTrimming)
			return linkDatabase.getParents(lo);

		/*
		 * long time = System.currentTimeMillis(); final Collection
		 * timplications = new LinkedList(); Collection parents =
		 * linkDatabase.getParents(lo); Iterator it = parents.iterator(); while
		 * (it.hasNext()) { Link parent = (Link) it.next(); if
		 * (TermUtil.isIntersection(parent)) continue; for (Link gp :
		 * linkDatabase.getParents(parent.getParent())) { if
		 * (TermUtil.isIntersection(gp)) continue; OBORestriction result = new
		 * OBORestrictionImpl(); if
		 * (ReasonerUtil.generateTransitiveImplication(reasoner, result, parent,
		 * gp)) { timplications.add(result); } } } VectorFilter filter = new
		 * VectorFilter() { public boolean satisfies(Object in) { Link inLink =
		 * (Link) in; if (!TermUtil.isImplied(inLink)) return true; Iterator it =
		 * timplications.iterator(); while (it.hasNext()) { Link pLink = (Link)
		 * it.next(); if (pLink.getType().equals(inLink.getType()) &&
		 * inLink.getParent().equals(pLink.getParent())) { return false; } }
		 * return true; } }; trimTime += System.currentTimeMillis() - time;
		 * Subset s = new Subset(filter, reasoner.getParents(lo), false); return
		 * s;
		 */
		VectorFilter<Link> filter = new VectorFilter<Link>() {
			public boolean satisfies(Link inLink) {
				boolean trim;
				if (normalTrimming)
					trim = !ReasonerUtil.shouldBeTrimmed(linkDatabase, inLink);
				else
					trim = !ReasonerUtil.shouldBeTrimmedNew(linkDatabase, inLink);

				//boolean trim = !ReasonerUtil.shouldBeTrimmedOld(linkDatabase, inLink);
				return trim;
			}
		};

		Collection<Link> parents = linkDatabase.getParents(lo);
		Collection<Link> out = new Subset<Link>(filter, parents, false);
		return out;
	}
	
	/*
	public Collection<Link> getChildren(LinkedObject lo) {
		if (!enableTrimming)
			return linkDatabase.getChildren(lo);

		Collection<Link> out = new ArrayList<Link>();
		for (Link link : linkDatabase.getChildren(lo)) {
			if (!ReasonerUtil.shouldBeTrimmed(linkDatabase, link))
				out.add(link);
		}
		return out;
	}

	public Collection<Link> getParents(LinkedObject lo) {
		if (!enableTrimming)
			return linkDatabase.getParents(lo);

		Collection<Link> out = new ArrayList<Link>();
		for (Link link : linkDatabase.getParents(lo)) {
			if (!ReasonerUtil.shouldBeTrimmed(linkDatabase, link))
				out.add(link);
		}
		return out;
	}
	*/


	public Collection<IdentifiedObject> getObjects() {
		if (linkDatabase == null){
			logger.debug("TrimmedLinkDatabase.getObjects -- linkDatabase ==null");
			return Collections.emptySet();
		}
		return linkDatabase.getObjects();
	}

	public IdentifiedObject getObject(String id) {
		return linkDatabase.getObject(id);
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}


	public TrimmedLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public boolean hasChildren(LinkedObject lo) {
		return linkDatabase.hasChildren(lo);
	}

	public boolean hasParents(LinkedObject lo) {
		return linkDatabase.hasParents(lo);
	}

}
