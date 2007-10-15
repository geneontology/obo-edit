package org.bbop.util;

import java.util.Set;

public interface Subsetable extends Set {

    public Set getUncachedSubset(VectorFilter filter);
    public Set getSubset(VectorFilter filter);
    public boolean removeSubset(Set set);
    public void markForRecategorize(Object o);
    public void recategorize(Object o);
    public void recache();
}
