package org.obo.filters;

/*
 * Returns the comment for a given
 * {@link org.oboedit.datamodel.IdentifiedObject }
 * belongs.
 *
 */

import java.util.Collection;

import org.obo.datamodel.*;

public class CommentSearchCriterion extends AbstractStringCriterion {

	public static final CommentSearchCriterion CRITERION = new CommentSearchCriterion();

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof CommentedObject)
			scratch.add(((CommentedObject) obj).getComment());
		return scratch;
	}

	public String getID() {
		return "comment";
	}
	
	@Override
	public int getMinCardinality() {
		return 0;
	}
	
	@Override
	public int getMaxCardinality() {
		return 1;
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Comment";
	}
}
