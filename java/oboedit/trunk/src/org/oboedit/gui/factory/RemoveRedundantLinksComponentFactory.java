package org.oboedit.gui.factory;

import org.apache.log4j.Logger;
import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.RemoveRedundantLinksComponent;



public class RemoveRedundantLinksComponentFactory extends 
AbstractComponentFactory<RemoveRedundantLinksComponent>{
	
	//initialize logger
	protected final static Logger logger = Logger.getLogger(RemoveRedundantLinksComponentFactory.class);
	public RemoveRedundantLinksComponentFactory() {
	}
	
	public String getID() {
		return "REMOVE REDUNDANT LINKS PANEL";
	}

	public RemoveRedundantLinksComponent doCreateComponent(String id) {
		return new RemoveRedundantLinksComponent(id);
	}

	public String getName() {
		return "Remove Redundant Links Panel";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.REASONER;
	}
	

}
