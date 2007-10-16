package org.bbop.util;

import java.util.*;

/**
 * A basic interface for grouping multiple sets into one collection.
 * Changes to the underlying sets will be immediately reflected in the
 * Superset. The superset is not necessarily a true set; there may be
 * duplicate items if an object exists in more than one of
 * the underlying sets.
 *
 */
public interface Superset<T> extends Set<T> {

    public void addSubset(Collection<? extends T> set, boolean modify);
    public void addSubset(Collection<? extends T> set);
    public void setModifier(Collection<? extends T> set);
    public boolean removeSubset(Collection<? extends T> set);
}
