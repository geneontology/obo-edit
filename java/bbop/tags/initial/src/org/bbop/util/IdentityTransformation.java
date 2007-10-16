package org.bbop.util;

/**
 * A VectorTransformer that transforms an object to itself
 */
public class IdentityTransformation implements VectorTransformer {

    /**
     * Transforms an object into itself
     * @param in the object to transform
     * @return the object given
     */
    public Object transform(Object in) {
	return in;
    }
}
