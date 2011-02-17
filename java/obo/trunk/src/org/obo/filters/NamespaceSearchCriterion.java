package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class NamespaceSearchCriterion extends AbstractStringCriterion {

	protected final static Logger logger = Logger.getLogger(NamespaceSearchCriterion.class);

        protected boolean excludeObsoletes = false;

	public NamespaceSearchCriterion() {
          this(false);
	}	

       public NamespaceSearchCriterion(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
       }

	public Collection getValues(Collection scratch, Object obj) {
          if (excludeObsoletes && isObsolete((IdentifiedObject) obj))
            return scratch;

		Namespace ns = ((IdentifiedObject) obj).getNamespace();
		if (ns != null)
			scratch.add(ns.getID());
		return scratch;
	}

	public String getID() {
		return "namespace";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Namespace";
	}
}
