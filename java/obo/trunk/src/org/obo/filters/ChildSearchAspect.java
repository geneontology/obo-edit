package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class ChildSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ChildSearchAspect.class);

	public ChildSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
                                     Filter traversalFilter, Object o) {
            // This method ends up getting called on a lot of built-in objects that can't be cast to LinkedObject,
            // e.g., xsd:integer
            // This was throwing an exception and the search was failing.
            // Now we check whether we can do the cast.
            LinkedObject lo;
            try {
                lo = (LinkedObject) o;
            } catch (ClassCastException e) {
                //                logger.debug("ChildSearchAspect.getObjects: can't cast " + o + ", class = " + o.getClass() + " to LinkedObject");
                return c;
            }

            // Until I fixed this in version 2.1-b13, a search for children
            // wasn't finding all the children unless the reasoner was on.
            // This turned out to be because TermUtil.getChildren wasn't working
            // right.  (With the reasoner on, the children were being gotten a
            // different way.)
		if (reasoner != null) {
//			for (Link link : reasoner.getChildren((LinkedObject) o)) 
//				c.add(link.getChild());
			LinkDatabase trimmedReasoner = new TrimmedLinkDatabase(reasoner);
                        Collection<Link> children = trimmedReasoner.getChildren(lo);
			for (Link link : children) 
                            c.add(link.getChild());
		} else {
                    c.addAll(TermUtil.getChildren(lo));
		}
		return c;
	}


	public boolean equals(Object o) {
          if (o == null) {
            return false;
          }
          return o.getClass().equals(getClass());
	}

	@Override
	public int hashCode() {
		return getClass().hashCode();
	}

	public String getID() {
		return "child";
	}

	public String toString() {
		return "Children";
	}

        // Not currently used
        public boolean requiresReasoner() {
          return false;
        }
}
