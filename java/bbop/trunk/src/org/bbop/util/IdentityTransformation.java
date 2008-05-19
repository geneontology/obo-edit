package org.bbop.util;

/**
 * A VectorTransformer that transforms an object to itself
 */
import org.apache.log4j.*;

public class IdentityTransformation implements VectorTransformer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IdentityTransformation.class);

    /**
     * Transforms an object into itself
     * @param in the object to transform
     * @return the object given
     */
    public Object transform(Object in) {
	return in;
    }
}
