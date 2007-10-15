package org.bbop.util;

import java.io.*;

public interface VectorFilter<T> extends Cloneable, Serializable {

    public boolean satisfies(T in);
}
