package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.VerificationManagerComponent;

import org.apache.log4j.*;

public class VerificationManagerFactory extends
	AbstractComponentFactory<VerificationManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(VerificationManagerFactory.class);
	public VerificationManagerFactory() {
	}
	
	public String getID() {
		return "VERIFICATION_MANAGER";
	}

	public VerificationManagerComponent doCreateComponent(String id) {
		return new VerificationManagerComponent(id);
	}

	public String getName() {
		return "Verification Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.TOOLS;
	}
}
