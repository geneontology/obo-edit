package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.OnTheFlyReasoner;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class ParentSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ParentSearchAspect.class);

	public ParentSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
			Filter traversalFilter, Object o) {
            LinkedObject lo;
            try {
                lo = (LinkedObject) o;
            } catch (ClassCastException e) {
                // In ChildSearchAspect, this happens a lot, but it doesn't seem to happen
                // in ParentSearchAspect, for some reason.
                logger.debug("ParentSearchAspect.getObjects: can't cast " + o + ", class = " + o.getClass() + " to LinkedObject");
                return c;
            }

		if(o instanceof LinkedObject){
			if(reasoner != null && !(reasoner instanceof OnTheFlyReasoner)){
				LinkDatabase trimmedReasoner = new TrimmedLinkDatabase(reasoner);
				for (Link link : trimmedReasoner.getParents((LinkedObject) o))
					if(traversalFilter == null || traversalFilter.satisfies(link))
						c.add(link.getParent());
			}
			else{
				c.addAll(TermUtil.getParents((LinkedObject) o, false, (LinkFilter) traversalFilter));
			}
		}
		return c;
	}


	public boolean equals(Object o) {
          if (o == null) {
//            logger.debug("ParentSearchAspect: o = null, class = " + getClass());
            return false;
          }
          return o.getClass().equals(getClass());
	}

	@Override
	public int hashCode() {
		return getClass().hashCode();
	}

	public String getID() {
		return "parent";
	}

	public String toString() {
		return "Parent";
	}

        // Not currently used
        public boolean requiresReasoner() {
          return false;
        }
}
