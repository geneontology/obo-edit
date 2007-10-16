package org.obo.util;

import java.util.Collection;

import org.bbop.util.TaskDelegate;
import org.obo.datamodel.OBOSession;
import org.obo.query.Query;
import org.obo.query.QueryEngine;

public class QueryUtil {
	
	public static <V> Collection<V> getResults(TaskDelegate<Collection<V>> task) {
		task.run();
		return task.getResults();
	}

	public static <T, V> Collection<V> query(QueryEngine engine,
			OBOSession session, Query<T, V> q) {
		TaskDelegate<Collection<V>> task = engine.query(session, q);
		return getResults(task);
	}

	public static <T, V> Collection<V> subquery(QueryEngine engine,
			Collection<? extends V> objects, Query<T, V> q) {
		TaskDelegate<Collection<V>> task = engine.subquery(objects, q);
		return getResults(task);

	}

	public static <T, V> Collection<V> query(QueryEngine engine,
			Collection<? extends T> objects, Query<T, V> q) {
		TaskDelegate<Collection<V>> task = engine.query(objects, q);
		return getResults(task);
	}

}
