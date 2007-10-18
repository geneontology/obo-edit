package org.obo.query.impl;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.bbop.util.TaskDelegate;
import org.obo.datamodel.OBOSession;
import org.obo.query.Query;
import org.obo.query.QueryResolver;

public abstract class CachingQueryResolver implements QueryResolver {

	protected Map<Object, Map<Query, Collection>> results = new HashMap<Object, Map<Query, Collection>>();

	protected class TaskDelegateWrapper<V> implements
			TaskDelegate<Collection<V>> {
		TaskDelegate<Collection<V>> task;
		Object o;
		Query q;

		public TaskDelegateWrapper(TaskDelegate<Collection<V>> task, Object o,
				Query q) {
			this.task = task;
			this.o = o;
			this.q = q;
		}
		
		public void cancel() {
			task.cancel();
		}

		public Collection<V> getResults() {
			return task.getResults();
		}

		public boolean isCancelled() {
			return task.isCancelled();
		}

		public boolean isRunning() {
			return task.isRunning();
		}

		public void run() {
			task.run();
				cacheResults(o, q, (Collection<V>) task.getResults());
		}

		public String getProgressString() {
			return task.getProgressString();
		}

		public Number getProgressValue() {
			return task.getProgressValue();
		}

		public Throwable getException() {
			return task.getException();
		}

		public boolean isFailed() {
			return task.isFailed();
		}
	}
	
	protected static class ResultsWrapper<V> implements TaskDelegate<Collection<V>> {

		protected Collection<V> results;
		
		public ResultsWrapper(Collection<V> results) {
			this.results = results;
		}
		
		public void cancel() {
		}

		public Collection<V> getResults() {
			return results;
		}

		public boolean isCancelled() {
			return false;
		}

		public boolean isRunning() {
			return false;
		}

		public void run() {
		}

		public String getProgressString() {
			return null;
		}

		public Number getProgressValue() {
			return null;
		}

		public Throwable getException() {
			return null;
		}

		public boolean isFailed() {
			return false;
		}		
	}

	public void clearCache() {
		results = new HashMap<Object, Map<Query, Collection>>();
	}

	public <T, V> TaskDelegate<Collection<V>> query(OBOSession session,
			Query<T, V> q, boolean cache) {
		final Collection<V> c = fetchCachedQuery(session, q);
		if (c == null) {
			final TaskDelegate<Collection<V>> task = query(session, q);
			if (cache) {
			TaskDelegate<Collection<V>> taskWrapper =
				new TaskDelegateWrapper<V>(task, session, q);
			return taskWrapper;
			} else
				return task;
		} else {
			return new ResultsWrapper<V>(c);
		}
	}

	protected void cacheResults(Object o, Query q, Collection c) {
		Map<Query, Collection> temp = results.get(o);
		if (temp == null) {
			temp = new HashMap<Query, Collection>();
			results.put(o, temp);
		}
		temp.put(q, c);
	}

	protected <V> Collection<V> fetchCachedQuery(Object o, Query q) {
		Map<Query, Collection> temp = results.get(o);
		if (temp != null) {
			Collection c = temp.get(q);
			if (c != null)
				return c;
		}
		return null;
	}

	public <T, V> TaskDelegate<Collection<V>> query(Collection<T> objects,
			Query<T, V> q, boolean cache) {
		final Collection<V> c = fetchCachedQuery(objects, q);
		if (c == null) {
			final TaskDelegate<Collection<V>> task = query(objects, q);
			if (cache) {
			TaskDelegate<Collection<V>> taskWrapper =
				new TaskDelegateWrapper<V>(task, objects, q);
			return taskWrapper;
			} else
				return task;
		} else {
			return new ResultsWrapper<V>(c);
		}
	}

	public <T, V> TaskDelegate<Collection<V>> subquery(Collection<V> objects,
			Query<T, V> q, boolean cache) {
		final Collection<V> c = fetchCachedQuery(objects, q);
		if (c == null) {
			final TaskDelegate<Collection<V>> task = subquery(objects, q);
			if (cache) {
			TaskDelegate<Collection<V>> taskWrapper =
				new TaskDelegateWrapper<V>(task, objects, q);
			return taskWrapper;
			} else
				return task;
		} else {
			return new ResultsWrapper<V>(c);
		}
	}
}
