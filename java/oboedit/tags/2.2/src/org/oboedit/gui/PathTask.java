package org.oboedit.gui;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import javax.swing.tree.TreePath;

import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.util.TermUtil;
import org.oboedit.util.PathUtil;

import org.apache.log4j.*;

public class PathTask extends AbstractTaskDelegate<Collection<TreePath>> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PathTask.class);
	protected LinkDatabase linkDatabase = DefaultLinkDatabase.getDefault();
	protected RootAlgorithm rootAlgorithm = RootAlgorithm.GREEDY;
	protected double progress;
	protected int currentTermIndex;
	protected Collection<LinkedObject> terms;
	protected static Integer i = 0;
	public void setTerms(Collection<LinkedObject> terms) {
		this.terms = terms;
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public void setRootAlgorithm(RootAlgorithm rootAlgorithm) {
		this.rootAlgorithm = rootAlgorithm;
	}

	@Override
	public void execute() throws Exception {
		Collection<TreePath> out = new LinkedList<TreePath>();
		Iterator<LinkedObject> it = terms.iterator();
		while(it.hasNext()) {
			LinkedObject term = it.next();
			progress = 0;
			setProgressString("Finding all paths to " + term);
			//logger.debug("PathTask: execute: term = " + term);
			rootAlgorithm.setLinkDatabase(linkDatabase);

			if (rootAlgorithm.isRoot(term)) {
				Object[] os = new Object[3];
				os[0] = PathUtil.ROOT;
				if (TermUtil.isObsolete(term))
					os[1] = PathUtil.OBSOLETE;
				else if (TermUtil.isProperty(term))
					os[1] = PathUtil.TYPES;
				else if (TermUtil.isClass(term))
					os[1] = PathUtil.CLASSES;
				else
					os[1] = PathUtil.INSTANCES;
				os[2] = new OBORestrictionImpl(term);
				TreePath path = new TreePath(os);
				out.add(path);
				
			} else {
				double incSize = 100 / TermUtil.getParentCount(linkDatabase,
						term);
				for (Link tr : linkDatabase.getParents(term)) {
					MultiMap<Link, TreePath> lookedAt = new MultiHashMap<Link, TreePath>();
					Map<Link, Integer> lookedAtCount = new HashMap<Link, Integer>();
					out.addAll(getPathsAsVector(incSize, tr, lookedAt, lookedAtCount));
					if (isCancelled()) {
						return;
					}
				}
			}
		}

		setResults(out);
		//logger.debug("PathTask: execute: out = " + out); 
	}

	protected Collection<TreePath> getPathsAsVector(double incSize, Link link,
			MultiMap<Link, TreePath> lookedAt, Map<Link, Integer> lookedAtCount) {

//		logger.debug("PathTask.getPathsAsVector calls counter: " + i);
		Collection<TreePath> out = new LinkedList<TreePath>();

		if (isCancelled())
			return out;

		Integer lookedAtNum = lookedAtCount.get(link);
//		logger.debug("lookedAtNum: " + lookedAtNum);
		if (lookedAtNum == null) {
			lookedAtNum = new Integer(0);
		}
		lookedAtNum = new Integer(lookedAtNum.intValue() + 1);
//		logger.debug("lookedAtNum: " + lookedAtNum);

		lookedAtCount.put(link, lookedAtNum);

		if (link.getParent() == null) {
			Object[] os = new Object[3];
			os[0] = PathUtil.ROOT;
			if (TermUtil.isObsolete(link.getChild()))
				os[1] = PathUtil.OBSOLETE;
			else if (TermUtil.isProperty(link.getChild()))
				os[1] = PathUtil.TYPES;
			else if (TermUtil.isClass(link.getChild()))
				os[1] = PathUtil.CLASSES;
			else
				os[1] = PathUtil.INSTANCES;
			os[2] = link;
			out.add(new TreePath(os));
			progress = progress + incSize;
//			setProgressValue((int) (progress / terms.size())
//					+ (100 * currentTermIndex / terms.size()));
			lookedAt.put(link, out);
			return out;
		}
		if (rootAlgorithm.isRoot(link.getParent())) {
			Object[] os = new Object[3];
			os[0] = PathUtil.ROOT;
			os[1] = PathUtil.CLASSES;
			os[2] = new OBORestrictionImpl(link.getParent());
			TreePath path = new TreePath(os);
			TreePath pathByAddingChild = path.pathByAddingChild(link);
			out.add(pathByAddingChild);
			progress = progress + incSize;
//			setProgressValue((int) (progress / terms.size())
//					+ (100 * currentTermIndex / terms.size()));
			lookedAt.put(link, out);
			return out;
		}
		if (lookedAt.containsKey(link)) {
			out = lookedAt.get(link);
			progress = progress + incSize;
//			setProgressValue((int) (progress / terms.size())
//					+ (100 * currentTermIndex / terms.size()));
			if (lookedAtNum.intValue() > 2)
				return Collections.emptySet();
			return out;
		}
		lookedAt.put(link, out);

		Collection<Link> links = linkDatabase.getParents(link.getParent());
		Collection<Link> filteredLinks = filterDisjointRels(links);
		for (Link parentRel : filteredLinks) {
			Collection<TreePath> parentVector = getPathsAsVector(incSize
					/ TermUtil.getParentCount(linkDatabase, link.getParent()), parentRel,
					lookedAt, lookedAtCount);
			for (TreePath path : parentVector) {
				TreePath pathByAddingChild = path.pathByAddingChild(link);
				if (!out.contains(pathByAddingChild))
					out.add(pathByAddingChild);
			}
		}
		i++;
		return out;
	}

	/**
	 * Looks at a collection containing a series of relationships like this:
	 * "cell adhesion --OBO_REL:is_a--> biological adhesion"
	 * and removes any that contain the relationship 'disjoint_from'.
	 * @param links
	 * @return linksClone
	 */
	protected Collection<Link> filterDisjointRels(Collection<Link> links) {
		Collection<Link> linksClone = links; 
		Iterator<Link> it = links.iterator();
		while (it.hasNext()){
			Link parentRel = it.next();
			if(parentRel.getType().getID().equals("disjoint_from")){
				//logger.debug("PathTask: filterDisjointRels: disjoint_from link");
				it.remove();
			} 
		} 
		return linksClone;
	}

}

