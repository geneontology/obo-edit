package org.obo.filters;

import java.util.Collection;
import java.util.Iterator;

import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;


import org.apache.log4j.*;

public class IsImpliedLinkCriterion extends AbstractBooleanLinkCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsImpliedLinkCriterion.class);

	protected ReasonedLinkDatabase reasoner;

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}

	public ReasonedLinkDatabase getReasoner() {
//		logger.debug("reasoner: " + reasoner);
		return reasoner;
	}


	/** 
	 * [AA] fix for is_implied search in LinkSearchPanel
	 * ((Impliable) link).isImplied() returns true only when checking condition through global filters where the LinkDatabaseCanvas gets FilteredLinkDatabase which sets these as reasoner made links.
	 * While searching for is_implied through the SearchComponent the OBORestrictionLinks have is_implied false (something weird going on with DefaultQueryResolver getting the LinkDatabase from the session..tried passing the ReasonedLinkDB there..implied still set to false on OBORestriction links )
	 * Condition below forces a check on these links to see if they are indeed not present in the ReasonedLinkDatabase. 
	 * */
	public boolean matches(Link o) {
		if (o instanceof Link && getReasoner() != null && !(o.isImplied())){
			Link link = o;
			for (OBOProperty oboProp : reasoner.getProperties()) {
				Collection<Link> reasonerLinks = reasoner.getLinks(oboProp);
				for (Link rlink : reasonerLinks) {
					if (rlink.equals(link)){
						return true;
					}
				}
			}
			return false;
		} else{
//			logger.debug("((Impliable) o).isImplied(): " + ((Impliable) o).isImplied());
			return ((Impliable) o).isImplied();
		}
	}

	public String getID() {
		return "is_implied";
	}

	@Override
	public String toString() {
		return "Is implied";
	}
}
