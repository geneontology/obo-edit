package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.DbxrefLibrary;

import org.apache.log4j.*;

public class DbxrefLibraryFactory extends AbstractComponentFactory<DbxrefLibrary> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefLibraryFactory.class);

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
		return FactoryCategory.METADATA;
	}

}
