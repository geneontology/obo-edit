package org.obo.datamodel;

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
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.util.TermUtil;

public class PathTask extends AbstractTaskDelegate<Collection<TreePath>> {
	protected LinkDatabase linkDatabase = DefaultLinkDatabase.getDefault();
	protected RootAlgorithm rootAlgorithm = RootAlgorithm.GREEDY;
	protected double progress;
	protected int currentTermIndex;
	protected Collection<LinkedObject> terms;

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
		currentTermIndex = 0;
		for (LinkedObject term : terms) {
			progress = 0;
			setProgressString("Finding all paths to " + term);
			rootAlgorithm.setLinkDatabase(linkDatabase);

			if (rootAlgorithm.isRoot(term)) {
				Object[] os = new Object[3];
				os[0] = TermModel.ROOT;
				if (TermUtil.isObsolete(term))
					os[1] = TermModel.OBSOLETE;
				else if (TermUtil.isProperty(term))
					os[1] = TermModel.TYPES;
				else if (TermUtil.isClass(term))
					os[1] = TermModel.CLASSES;
				else
					os[1] = TermModel.INSTANCES;
				os[2] = new OBORestrictionImpl(term);
				TreePath path = new TreePath(os);
				out.add(path);
			} else {
				double incSize = 100 / TermUtil.getParentCount(linkDatabase,
						term);
				for (Link tr : linkDatabase.getParents(term)) {
					MultiMap<Link, TreePath> lookedAt = new MultiHashMap<Link, TreePath>();
					Map lookedAtCount = new HashMap();
					out.addAll(getPathsAsVector(incSize, tr, lookedAt,
							lookedAtCount));
					if (isCancelled()) {
						return;
					}
				}
			}
			currentTermIndex++;
		}
		setResults(out);
	}

	protected Collection<TreePath> getPathsAsVector(double incSize, Link link,
			MultiMap<Link, TreePath> lookedAt, Map lookedAtCount) {
		if (isCancelled())
			return Collections.emptyList();

		Integer lookedAtNum = (Integer) lookedAtCount.get(link);
		if (lookedAtNum == null) {
			lookedAtNum = new Integer(0);
		}
		lookedAtNum = new Integer(lookedAtNum.intValue() + 1);
		lookedAtCount.put(link, lookedAtNum);

		if (link.getParent() == null) {
			Object[] os = new Object[3];
			os[0] = TermModel.ROOT;
			if (TermUtil.isObsolete(link.getChild()))
				os[1] = TermModel.OBSOLETE;
			else if (TermUtil.isProperty(link.getChild()))
				os[1] = TermModel.TYPES;
			else if (TermUtil.isClass(link.getChild()))
				os[1] = TermModel.CLASSES;
			else
				os[1] = TermModel.INSTANCES;
			os[2] = link;
			lookedAt.add(link, new TreePath(os));
			progress = progress + incSize;
			setProgressValue((int) (progress / terms.size())
					+ (100 * currentTermIndex / terms.size()));
			return lookedAt.get(link);
		}
		if (rootAlgorithm.isRoot(link.getParent())) {
			Object[] os = new Object[3];
			os[0] = TermModel.ROOT;
			os[1] = TermModel.CLASSES;
			os[2] = new OBORestrictionImpl(link.getParent());
			TreePath path = new TreePath(os);
			TreePath pathByAddingChild = path.pathByAddingChild(link);
			progress = progress + incSize;
			setProgressValue((int) (progress / terms.size())
					+ (100 * currentTermIndex / terms.size()));

			lookedAt.add(link, pathByAddingChild);
			return lookedAt.get(link);
		}

		if (lookedAt.containsKey(link)) {
			progress = progress + incSize;
			setProgressValue((int) (progress / terms.size())
					+ (100 * currentTermIndex / terms.size()));
			if (lookedAtNum.intValue() > 2)
				return Collections.emptySet();
			return lookedAt.get(link);
		}

		Iterator it = linkDatabase.getParents(link.getParent()).iterator();
		while (it.hasNext()) {
			Link parentRel = (Link) it.next();
			Collection parentVector = getPathsAsVector(incSize
					/ TermUtil.getParentCount(linkDatabase, link.getParent()),
					parentRel, lookedAt, lookedAtCount);
			Iterator it2 = parentVector.iterator();
			while (it2.hasNext()) {
				TreePath path = (TreePath) it2.next();
				TreePath pathByAddingChild = path.pathByAddingChild(link);
				if (!lookedAt.get(link).contains(pathByAddingChild))
					lookedAt.add(link, pathByAddingChild);
			}
		}
		return lookedAt.get(link);
	}

}
