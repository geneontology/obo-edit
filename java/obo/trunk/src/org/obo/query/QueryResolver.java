package org.obo.query;

import java.util.Collection;

import org.bbop.util.TaskDelegate;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.OBOSession;

public interface QueryResolver {

	public <T, V> TaskDelegate<Collection<V>> query(LinkDatabase linkDatabase, Query<T, V> q);

//	public <T, V> TaskDelegate<Collection<V>> query(OBOSession session, Query<T, V> q);

	public <T, V> TaskDelegate<Collection<V>> subquery(Collection<? extends V> objects, Query<T, V> q);

	public <T, V> TaskDelegate<Collection<V>> query(Collection<? extends T> objects, Query<T, V> q);

	public <T, V> boolean wantsQuery(Query<T, V> q);
}
