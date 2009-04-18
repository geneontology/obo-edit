package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.AssertLinksComponent;
import org.apache.log4j.*;


public class AssertLinksComponentFactory extends 
AbstractComponentFactory<AssertLinksComponent>{
	
	//initialize logger
	protected final static Logger logger = Logger.getLogger(AssertLinksComponentFactory.class);
	public AssertLinksComponentFactory() {
	}
	
	public String getID() {
		return "ASSERT IMPLIED LINKS PANEL";
	}

	public AssertLinksComponent doCreateComponent(String id) {
		return new AssertLinksComponent(id);
	}

	public String getName() {
		return "Assert Implied Links Panel - Work In Progress";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.REASONER;
	}
	
}

