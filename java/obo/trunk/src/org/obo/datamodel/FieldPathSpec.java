package org.obo.datamodel;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.obo.filters.SearchCriterion;

import org.apache.log4j.*;

public class FieldPathSpec {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FieldPathSpec.class);

	protected List<SearchCriterion> elements = new LinkedList<SearchCriterion>();

	public FieldPathSpec(FieldPath path) {
		for(FieldPath.FieldPathElement e : path.getElements()) {
			elements.add(e.getField());
		}
	}
	
	public FieldPathSpec(SearchCriterion... newElements) {
		readElements(newElements);
	}

	public FieldPathSpec(FieldPathSpec parentPath,
			SearchCriterion... newElements) {
		for (SearchCriterion e : parentPath.getElements()) {
			elements.add(e);
		}
		readElements(newElements);
	}
	
	public FieldPathSpec getParent() {
		if (elements.size() == 0) {
			return null;
		}
		SearchCriterion [] vals = new SearchCriterion[elements.size() - 1];
		for(int i=0; i < vals.length; i++)
			vals[i] = elements.get(i);
		return new FieldPathSpec(vals);
	}

	protected void readElements(SearchCriterion[] newElements) {
		for (SearchCriterion sc : newElements) {
			elements.add(sc);
		}
	}
	
	public static Collection<FieldPath> createQueryPaths(Collection<FieldPathSpec> specs, IdentifiedObject io) {
		Collection<FieldPath> out = new LinkedList<FieldPath>();
		for(FieldPathSpec spec : specs) {
			out.add(createQueryPath(spec, io));
		}
		return out;
	}
	
	public static FieldPath createQueryPath(FieldPathSpec spec) {
		return createQueryPath(spec, null);
	}

	public static FieldPath createQueryPath(FieldPathSpec spec, IdentifiedObject io) {
		LinkedList<FieldPath.FieldPathElement> elements =
			new LinkedList<FieldPath.FieldPathElement>();
		for(SearchCriterion sc : spec.getElements()) {
			elements.add(new FieldPath.FieldPathElement(sc, null));
		}
		FieldPath out = new FieldPath(io);
		out.elements = elements;
		return out;
	}

	public FieldPath createQueryPath(IdentifiedObject io) {
		return createQueryPath(this, io);
	}
	
	public List<SearchCriterion> getElements() {
		return elements;
	}
	
	public SearchCriterion getLastCriterion() {
		return elements.get(elements.size() - 1);
	}
	
	public String toString() {
		return elements.toString();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof FieldPathSpec) {
			FieldPathSpec spec = (FieldPathSpec) obj;
			if (spec.getElements().size() != getElements().size())
				return false;
			for(int i=0; i < spec.getElements().size(); i++) {
				SearchCriterion sc = spec.getElements().get(i);
				SearchCriterion sc2 = getElements().get(i);
				if (!sc.equals(sc2))
					return false;
			}
			return true;
		} else
			return false;
	}
	
	@Override
	public int hashCode() {
		return elements.hashCode();
	}
}
