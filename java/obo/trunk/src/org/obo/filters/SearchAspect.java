package org.obo.filters;

import java.util.*;

import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface SearchAspect extends Cloneable {

	public Collection<LinkedObject> getObjects(Collection<LinkedObject> c,
			ReasonedLinkDatabase reasoner, Filter traversalFilter, Object o);
	public String getID();

        // 5/9/11: This method was used for an experiment where search aspects that required the reasoner were
        // left off the aspect menu if the reasoner was off.  It is not currently used and could probably be removed.
	public boolean requiresReasoner();
  }
