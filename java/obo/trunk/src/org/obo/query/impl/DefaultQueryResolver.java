package org.obo.query.impl;

import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.ProgressValued;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOSession;
import org.obo.filters.TermParentSearchCriterion;
import org.obo.query.Query;
import org.obo.query.QueryResolver;

import org.apache.log4j.*;

public class DefaultQueryResolver implements QueryResolver {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultQueryResolver.class);

	public static DefaultQueryResolver resolver;

	public static QueryResolver getResolver() {
		if (resolver == null)
			resolver = new DefaultQueryResolver();
		return resolver;
	}

	public boolean wantsQuery(Query q) {
		return true;
	}

	protected boolean queryMightAccept(Query q, Class<?> c) {
		boolean might = c.isAssignableFrom(q.getInputType())
				|| queryAccepts(q, c);
		return might;
	}

	protected boolean queryAccepts(Query q, Class<?> c) {
//		logger.debug("queryAccepts Class c: " + c);
		Class<?> inputType = q.getInputType();
		boolean isAssignable = inputType.isAssignableFrom(c);
		// boolean isAssignable = c.isAssignableFrom(inputType);
		return isAssignable;
	}

	public <T, V> TaskDelegate<Collection<V>> query(final LinkDatabase linkDatabase,
			final Query<T, V> q) {
		TaskDelegate<Collection<V>> out = new AbstractTaskDelegate<Collection<V>>() {
			@Override
			public void execute() {
				try {
					long time = System.currentTimeMillis(); // DEL					
					logger.info("Starting search: " + q);
					progressString = "Querying...";
					Collection<V> out = getResultHolder(q);
					results = out;
					Collection<FieldPathSpec> specs = q.getInputPaths();
					if (specs == null) {
						specs = new LinkedList<FieldPathSpec>();
						specs.add(new FieldPathSpec());
					}
					Collection<FieldPath> paths = new LinkedList<FieldPath>();
					for (FieldPathSpec spec : specs) {
						FieldPath qpath = FieldPathSpec.createQueryPath(spec);
						if (cancelled)
							return;
					
						Collection<FieldPath> values = FieldPath.resolve(qpath,
								linkDatabase);
						
						for (FieldPath p : values) {
//							logger.debug("queryAccepts(q, p.getLastValue().getClass()): " + queryAccepts(q, p.getLastValue().getClass()));
//							logger.debug("p.getObject().isBuiltIn(): " + p.getObject().isBuiltIn());
							if (queryAccepts(q, p.getLastValue().getClass())
							    // Don't include built-ins like "xsd:date" or "obo:TERM"
							    && !(p.getObject()).isBuiltIn()) {
//							    logger.debug("DefaultQueryResolver.execute adding FieldPath " + p); 
								paths.add(p);
							}
						}
					}
					int total = paths.size();
					int i = 0;
					for (FieldPath path : paths) {
						progress = new Integer(100 * i / total);
						if (cancelled)
							return;
						Object vgg = path.getLastValue();

						q.setFieldPath(path);
						Object r = q.matches((T) vgg);
						V result = (V) r;
						if (result != null) {
							out.add(result);
						}
						i++;
					}
					if (out instanceof List && q.getComparator() != null) {
						Collections.sort((List<V>) out, q.getComparator());
					}
					logger.info("Finished search, found " + out.size() + " results in " + (System.currentTimeMillis() - time)+"" + " ms"); // DEL
					results = out;
				} catch (Throwable t) {
					t.printStackTrace();
				}

			}

		};
		return out;
	}

	public <T, V> TaskDelegate<Collection<V>> query(
			final Collection<? extends T> objects, final Query<T, V> q) {
		TaskDelegate<Collection<V>> out = new AbstractTaskDelegate<Collection<V>>() {

			@Override
			public void execute() {
				long time = System.currentTimeMillis(); // DEL
				progressString = "Querying...";
				Collection<V> out = getResultHolder(q);
				int i = 0;
				for (T o : objects) {
					if (cancelled)
						return;
					V result = q.matches(o);
					progress = new Integer(100 * i / objects.size());
					if (result != null) {
						out.add(result);
					}
					i++;
				}
				if (out instanceof List && q.getComparator() != null) {
					Collections.sort((List<V>) out, q.getComparator());
				}
//				logger.debug("Search " + q + " found " + out.size() + " results in " + (System.currentTimeMillis() - time) + " ms"); // DEL
				results = out;
			}

		};
		return out;
	}

	public <T, V> TaskDelegate<Collection<V>> subquery(
			final Collection<? extends V> objects, final Query<T, V> q) {
		TaskDelegate<Collection<V>> out = new AbstractTaskDelegate<Collection<V>>() {

			@Override
			public void execute() {
				Collection<V> out = getResultHolder(q);
				for (V o : objects) {
					V result = q.matches(q.convertToInputType(o));
					if (result != null) {
						out.add(result);
					}
				}
				if (out instanceof List && q.getComparator() != null) {
					Collections.sort((List<V>) out, q.getComparator());
				}
				results = out;
			}

		};
		return out;
	}

	public <T, V> Collection<V> getResultHolder(Query<T, V> q) {
		Collection<V> out = q.createResultHolder();
		if (out == null)
			out = new ArrayList<V>();
		return out;
	}
}
