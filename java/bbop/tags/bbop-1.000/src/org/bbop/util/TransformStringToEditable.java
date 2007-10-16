package org.bbop.util;

/**
 * Transformation that turns a java.lang.String object into a
 * org.bbop.util.EditableString
 */
public class TransformStringToEditable implements VectorTransformer {

    public Object transform(Object in) {
	if (in instanceof String) {
	    return new EditableString((String) in);
	} else
	    return in;
    }

}
