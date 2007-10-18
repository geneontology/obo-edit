package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.VerificationManagerComponent;

public class VerificationManagerFactory extends
		AbstractComponentFactory<VerificationManagerComponent> {
	public VerificationManagerFactory() {
		addID("VERIFICATION_MANAGER");
		addID("plugin:org.geneontology.oboedit.plugin.VerificationPlugin");
	}

	public VerificationManagerComponent doCreateComponent(String id) {
		return new VerificationManagerComponent(id);
	}

	public String getName() {
		return "Verification Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}
}
