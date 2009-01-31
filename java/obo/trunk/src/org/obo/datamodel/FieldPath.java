package org.obo.datamodel;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.bbop.util.EmptyIterator;
import org.bbop.util.IteratorFactory;
import org.bbop.util.ObjectUtil;
import org.bbop.util.SingletonIterator;
import org.bbop.util.SuperCollection;
import org.bbop.util.TransformingIterator;
import org.bbop.util.VectorTransformer;
import org.obo.filters.SearchCriterion;
import org.obo.filters.SynonymSearchCriterion;
import org.obo.filters.SynonymTextSearchCriterion;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class FieldPath {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FieldPath.class);

	public static class FieldPathElement {
		protected SearchCriterion field;

		protected Object value;

		public FieldPathElement(SearchCriterion field, Object value) {
			super();
			this.field = field;
			this.value = value;
		}

		public SearchCriterion getField() {
			return field;
		}

		public Object getValue() {
			return value;
		}
	}

	public static final Object EMPTY = new Object();

	protected IdentifiedObject object;

	protected LinkedList<FieldPathElement> elements = new LinkedList<FieldPathElement>();

	public FieldPath(IdentifiedObject object, Object... newElements) {
		this(object);
		readElements(newElements);
	}

	public Object getValueAt(FieldPathSpec spec) {
		if (!startsWithSpec(spec))
			return null;
		return elements.get(spec.getElements().size() - 1).getValue();
	}

	public Object getLastValue() {
		if (elements.size() > 0)
			return elements.getLast().getValue();
		else
			return object;
	}

	public SearchCriterion getLastField() {
		return elements.getLast().getField();
	}

	protected FieldPath() {
	}

	public FieldPath(FieldPathSpec spec, Object... newElements) {
		for (int i = 0; i < newElements.length; i++) {
			if (i == 0) {
				this.object = (IdentifiedObject) newElements[0];
			} else {
				elements.add(new FieldPathElement(
						spec.getElements().get(i - 1), newElements[i]));
			}
		}
	}

	public FieldPath(FieldPath parentPath, Object... newElements) {
		this(parentPath.getObject());
		if (newElements.length % 2 != 0)
			throw new IllegalArgumentException(
					"Even number of arguments required");
		for (FieldPathElement e : parentPath.getElements()) {
			elements.add(e);
		}
		readElements(newElements);
	}

	protected void readElements(Object[] newElements) {
		for (int i = 0; i < newElements.length; i += 2) {
			Object o = newElements[i];
			if (o instanceof SearchCriterion) {
				elements.add(new FieldPathElement((SearchCriterion) o,
						newElements[i + 1]));
			} else
				throw new IllegalArgumentException(
						"Field identifier should be a " + "SearchCriterion");
		}
	}

	public boolean startsWithSpec(FieldPathSpec spec) {
		int i;
		for (i = 0; i < elements.size(); i++) {
			if (i >= spec.getElements().size())
				return true;
			FieldPathElement e = elements.get(i);
			if (!e.getField().equals(spec.getElements().get(i)))
				return false;
		}
		return i >= spec.getElements().size();
	}

	public boolean matchesSpec(FieldPathSpec spec) {
		if (elements.size() != spec.getElements().size())
			return false;
		for (int i = 0; i < elements.size(); i++) {
			FieldPathElement e = elements.get(i);
			if (!e.getField().equals(spec.getElements().get(i)))
				return false;
		}
		return true;
	}

	public FieldPathSpec getSpec() {
		return new FieldPathSpec(this);
	}

	public FieldPath getParentPath() {
		if (elements.size() == 0)
			return null;
		FieldPath out = new FieldPath(getObject());
		for (int i = 0; i < elements.size() - 1; i++)
			out.getElements().add(getElements().get(i));
		return out;
	}

	public Collection<FieldPath> resolve() {
		return resolve(this, null);
	}

	protected static class FieldPathRootIteratorFactory implements
			IteratorFactory {

		protected LinkDatabase linkDatabase;

		public FieldPathRootIteratorFactory(LinkDatabase linkDatabase) {
			this.linkDatabase = linkDatabase;
		}

		public Iterator getIterator(Object object) {
			if (object != null) {
				return new SingletonIterator(new FieldPath(
						(IdentifiedObject) object));
			} else {
				return new TransformingIterator<IdentifiedObject, FieldPath>(
						linkDatabase.getObjects().iterator(),
						new VectorTransformer<IdentifiedObject, FieldPath>() {

							public FieldPath transform(IdentifiedObject in) {
								return new FieldPath(in);
							}
						});
			}
		}

	}

	protected static class FieldPathElementIteratorFactory implements
			IteratorFactory<FieldPath, FieldPath> {

		protected FieldPathElement element;

		public FieldPathElementIteratorFactory(FieldPathElement element) {
			this.element = element;
		}

		public Iterator<FieldPath> getIterator(FieldPath p) {
			FieldPathElement object = element;
			if (ObjectUtil.equals(object.getValue(), EMPTY)) {
				return EmptyIterator.emptyIterator();
			} else if (object.getValue() != null) {
				FieldPath parent = p;
				FieldPath path = new FieldPath(parent, object.getField(),
						object.getValue());
				return new SingletonIterator(path);

			} else {
				final FieldPath parent = p;
				final SearchCriterion crit = object.getField();
				final Object lastValue = parent.getLastValue();
				Collections.emptySet();
				if (ObjectUtil.equals(lastValue, EMPTY))
					return EmptyIterator.emptyIterator();
				final Collection values = crit.getValues(new LinkedList(),
						lastValue);
				if (values.size() == 0) {
					return new SingletonIterator(new FieldPath(parent, crit,
							EMPTY));
				} else
					return new TransformingIterator<Object, FieldPath>(values
							.iterator(),
							new VectorTransformer<Object, FieldPath>() {

								public FieldPath transform(Object in) {
									FieldPath path = new FieldPath(parent,
											crit, in);
									return path;
								}
							});
			}
		}
	}

	public static void main(String[] args) throws Exception {
		OBOSession session = TermUtil
				.getSession("/Users/jrichter/ontology/so-xp.obo");
		FieldPath path = new FieldPath((IdentifiedObject) null);
		FieldPath path2 = new FieldPath((IdentifiedObject) null,
				new SynonymSearchCriterion(), null,
				new SynonymTextSearchCriterion(), null);

		Collection<FieldPath> c1 = FieldPath.resolve(path, session
				.getLinkDatabase());
		Collection<FieldPath> c2 = FieldPath.resolve(path2, session
				.getLinkDatabase());
		logger.info("c1.size = " + c1.size());
		logger.info("c2.size = " + c2.size());
		/*
		 * for (FieldPath p : c2) { logger.info(p.toString()); }
		 */
	}

	public static Collection<FieldPath> resolve(FieldPathSpec spec,
			LinkDatabase linkDatabase) {
		FieldPath queryPath = FieldPathSpec.createQueryPath(spec);
		return resolve(queryPath, linkDatabase);
	}

	public static Collection<FieldPath> resolve(FieldPath path,
			LinkDatabase linkDatabase) {
		List<IteratorFactory> factories = new LinkedList<IteratorFactory>();
		factories.add(new FieldPathRootIteratorFactory(linkDatabase));
//		logger.debug("FieldPath path.elements.size(): " + path.elements.size());
		for (FieldPathElement elt : path.elements) {
			try{
				factories.add(new FieldPathElementIteratorFactory(elt));
			} catch(Exception e){
				logger.debug("caught exception while adding new FieldPathElement: " + e);
			}

		}
		return new SuperCollection<IdentifiedObject, FieldPath>(path
				.getObject(), factories);
	}

	/*
	 * public static Collection<FieldPath> resolve(FieldPath path, LinkDatabase
	 * linkDatabase) { if (path.getElements().size() == 0 || path.getLastValue() !=
	 * null) { return Collections.singleton(path); } Collection<FieldPath> out =
	 * new LinkedList(); Collection<FieldPath> parentPaths =
	 * resolve(path.getParentPath(), linkDatabase); SearchCriterion crit =
	 * path.getLastField(); for (FieldPath parentPath : parentPaths) { if
	 * (ObjectUtil.equals(parentPath.getLastValue(), EMPTY)) continue;
	 * Collection values = crit.getValues(new LinkedList(), parentPath
	 * .getLastValue()); if (values.size() == 0) { out.add(new
	 * FieldPath(parentPath, crit, EMPTY)); } else { for (Object value : values) {
	 * out.add(new FieldPath(parentPath, crit, value)); } } } return out; }
	 */
	public static void coalesce(List<FieldPath> paths) {
		int originalSize = paths.size();
		for (int i = paths.size() - 1; i >= 1; i--) {
			if (paths.get(i) != null)
				coalesce(i, paths);
		}
		Iterator it = paths.iterator();
		while (it.hasNext()) {
			if (it.next() == null)
				it.remove();
		}
	}

	protected static void coalesce(int index, List<FieldPath> paths) {
		FieldPath f = paths.get(index);
		for (int i = index; i >= 0; i--) {
			if (index == i)
				continue;
			if (paths.get(i) != null) {
				if (collapsable(f, paths.get(i))) {
					paths.set(i, null);
				}
			}
		}
	}

	protected static boolean collapsable(FieldPath f, FieldPath path) {
		if (f.getLength() == path.getLength()) {
			for (int i = 0; i < f.getLength(); i++) {
				FieldPath.FieldPathElement e1 = f.getElements().get(i);
				FieldPath.FieldPathElement e2 = path.getElements().get(i);
				if (!e1.getField().equals(e2.getField())) {
					return false;
				}
				// if the cardinality <= 1, always coalesce, because that means
				// we just replace a singleton value when we coalesce anyway
				if (e1.getField().getMaxCardinality() <= 1)
					continue;
				// if the values are equal, they always match
				if (e1.getValue().equals(e2.getValue()))
					continue;
				// if the values aren't equal but have the same identifiable
				// object ids, they match
				if (e1.getField().getMaxCardinality() > 1) {
					if (!(e1.getValue() instanceof IdentifiableObject && e2
							.getValue() instanceof IdentifiableObject))
						return false;
					IdentifiableObject io1 = (IdentifiableObject) e1.getValue();
					IdentifiableObject io2 = (IdentifiableObject) e2.getValue();
					if (!io1.getID().equals(io2.getID())) {
						return false;
					}
				}
			}
			return true;
		} else
			return false;
	}

	public FieldPath(IdentifiedObject object) {
		this.object = object;
	}
	
	public boolean containsValue(Object value) {
		for(FieldPathElement e : getElements()) {
			if (e.getValue().equals(value))
				return true;
		}
		return false;
	}

	public List<FieldPathElement> getElements() {
		return elements;
	}

	public int getLength() {
		return elements.size();
	}

	public IdentifiedObject getObject() {
		return object;
	}

	@Override
	public String toString() {
		StringBuffer out = new StringBuffer("FieldPath(");
		if (getObject() == null)
			out.append("*");
		else
			out.append(getObject().getID());
		out.append(" -> ");
		int index = 0;
		for (FieldPathElement e : getElements()) {
			if (index > 0)
				out.append(", ");
			out.append("<" + e.getField() + "," + e.getValue() + ">");
			index++;
		}
		out.append(")");
		return out.toString();
	}

	@Override
	public int hashCode() {
		/*
		 * int hash = (object == null ? 1 : object.hashCode()); for
		 * (FieldPathElement e : elements) { if (e.getValue() instanceof
		 * IdentifiableObject) { hash += ((IdentifiableObject)
		 * e.getValue()).getID().hashCode(); } else if
		 * (e.getField().getMaxCardinality() <= 1) { hash += 0; } else hash +=
		 * e.getValue().hashCode(); hash <<= 2; } return hash;
		 * 
		 */
		int hash = (object == null ? 1 : object.hashCode());
		for (FieldPathElement e : elements) {
			hash += e.getValue().hashCode();
			hash <<= 2;
		}
		return hash;
	}

	public boolean equals(Object o) {
		/*
		 * if (o instanceof FieldPath) { boolean b = collapsable(((FieldPath)
		 * o), this); return b; } else return false;
		 */
		if (o instanceof FieldPath) {
			FieldPath path = (FieldPath) o;
			if (path.elements.size() != elements.size())
				return false;
			for(int i=0; i < elements.size(); i++) {
				FieldPathElement e1 = elements.get(i);
				FieldPathElement e2 = path.elements.get(i);
				if (!ObjectUtil.equals(e1.getValue(), e2.getValue()) ||
						!ObjectUtil.equals(e1.getField(), e2.getField()))
						return false;
			}
			return true;
		} else
			return false;
	}
}
