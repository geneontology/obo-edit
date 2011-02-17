package org.obo.filters;

/*
 * Returns the comment for a given
 * {@link org.obo.datamodel.IdentifiedObject }
 */

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class CommentSearchCriterion extends AbstractStringCriterion {
	protected final static Logger logger = Logger.getLogger(CommentSearchCriterion.class);

	public static final CommentSearchCriterion CRITERION = new CommentSearchCriterion();

        protected boolean excludeObsoletes = false;

	public CommentSearchCriterion() {
          this(false);
	}	

       public CommentSearchCriterion(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
       }

	public Collection getValues(Collection scratch, Object obj) {
          // This is adding a lot of empty comments, but leaving them out doesn't seem to make much difference later on...
//          if (obj instanceof CommentedObject && !((CommentedObject) obj).getComment().equals("")) {
          if (obj instanceof CommentedObject)
            if (!(excludeObsoletes && isObsolete((IdentifiedObject) obj)))
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
