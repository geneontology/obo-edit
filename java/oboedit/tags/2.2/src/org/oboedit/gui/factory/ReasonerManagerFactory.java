package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ReasonerManagerComponent;

import org.apache.log4j.*;

public class ReasonerManagerFactory extends AbstractComponentFactory<ReasonerManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerManagerFactory.class);

	public ReasonerManagerFactory() {
	}
	
	public String getID() {
		return "REASONER_MANAGER";
	}

	public ReasonerManagerComponent doCreateComponent(String id) {
		return new ReasonerManagerComponent(id);
	}

	public String getName() {
		return "Reasoner Manager";
	}
	
	@Override
	public boolean getPreferSeparateWindow() {
		return true;
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.REASONER;
	}

	@Override
	public String getHelpTopicID() {
		return "The_Reasoner_and_the_Interface";
	}

}
