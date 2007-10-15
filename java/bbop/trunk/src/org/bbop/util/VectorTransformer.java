package org.bbop.util;

public interface VectorTransformer<IN, OUT> {

    public OUT transform(IN in);
}
