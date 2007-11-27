package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.DbxrefLibrary;

public class DbxrefLibraryFactory extends AbstractComponentFactory<DbxrefLibrary> {

	public DbxrefLibraryFactory() {
	}
	
	public String getID() {
		return "DBXREF_LIBRARY";
	}

	public DbxrefLibrary doCreateComponent(String id) {
		return new DbxrefLibrary(id);
	}

	public String getName() {
		return "Dbxref Library";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.TOOLS;
	}

}
