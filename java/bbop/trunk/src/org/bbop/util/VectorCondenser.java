package org.bbop.util;

public interface VectorCondenser {

    public static final Object REMOVE_OP = new Object();

    public static final Pair REMOVE_PAIR = new Pair(REMOVE_OP,
						    REMOVE_OP);

    public Pair condense(Object a, Object b);
}
