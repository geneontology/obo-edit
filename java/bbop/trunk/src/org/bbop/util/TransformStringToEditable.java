package org.bbop.util;

/**
 * Transformation that turns a java.lang.String object into a
 * org.bbop.util.EditableString
 */
import org.apache.log4j.*;

public class TransformStringToEditable implements VectorTransformer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransformStringToEditable.class);

    public Object transform(Object in) {
	if (in instanceof String) {
	    return new EditableString((String) in);
	} else
	    return in;
    }

}
