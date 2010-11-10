package org.geneontology.gold.io.postgres.test;

import java.io.FileNotFoundException;
import java.sql.SQLException;

import org.geneontology.gold.io.postgres.SchemaManager;

import junit.framework.TestCase;

public class SchemaManagerTest extends TestCase {

	
	public static void testLoadSchema() throws ClassNotFoundException, SQLException, FileNotFoundException{
		SchemaManager sm = new SchemaManager();

		sm.loadSchemaSQL("localhost", "postgres", "postgres", "temp12", "sql/ontol.sql", "", false);
	}
	
}
