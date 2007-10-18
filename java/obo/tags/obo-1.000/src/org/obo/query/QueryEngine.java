package org.obo.query;

import java.util.LinkedList;
import java.util.List;
import java.util.Collection;

import org.bbop.util.TaskDelegate;
import org.obo.datamodel.OBOSession;
import org.obo.query.impl.CachingQueryResolver;
import org.obo.query.impl.DefaultQueryResolver;
import org.obo.util.QueryUtil;

public class QueryEngine extends CachingQueryResolver {

	protected OBOSession session;

	protected List<QueryResolver> queryResolvers = new LinkedList<QueryResolver>();

	public QueryEngine(OBOSession session) {
		setSession(session);
	}

	public OBOSession getSession() {
		return session;
	}

	public void addQueryResolver(QueryResolver queryResolver) {
		queryResolvers.add(queryResolver);
	}

	public void removeQueryResolver(QueryResolver queryResolver) {
		queryResolvers.remove(queryResolver);
	}

	public void setSession(OBOSession session) {
		if (this.session != null && session.getQueryResolver() != null)
			removeQueryResolver(session.getQueryResolver());
		this.session = session;
		if (session.getQueryResolver() != null)
			queryResolvers.add(0, session.getQueryResolver());
	}

	public <T, V> Collection<V> query(Query<T, V> q) {
		return query(q, false);
	}

	public <T, V> Collection<V> query(Query<T, V> q, boolean cache) {
		return QueryUtil.getResults(query(session, q, cache));
	}

	protected QueryResolver getResolver(Query q) {
		for (QueryResolver resolver : queryResolvers) {
			if (resolver.wantsQuery(q))
				return resolver;
		}
		return DefaultQueryResolver.getResolver();
	}

	public <T, V> TaskDelegate<Collection<V>> query(OBOSession session,
			Query<T, V> q) {
		QueryResolver resolver = getResolver(q);
		return resolver.query(session, q);
	}

	public <T, V> TaskDelegate<Collection<V>> query(
			Collection<? extends T> objects, Query<T, V> q) {
		QueryResolver resolver = getResolver(q);
		return resolver.query(objects, q);
	}

	public <T, V> TaskDelegate<Collection<V>> subquery(
			Collection<? extends V> objects, Query<T, V> q) {
		QueryResolver resolver = getResolver(q);
		return resolver.subquery(objects, q);
	}

	public boolean wantsQuery(Query q) {
		return true;
	}
}
