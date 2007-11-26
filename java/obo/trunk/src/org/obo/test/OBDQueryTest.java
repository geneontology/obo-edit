package org.obo.test;

import java.io.IOException;
import java.io.PrintStream;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.OBDSQLDatabaseAdapter;
import org.obo.dataadapter.OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.datamodel.impl.OBOSessionImpl;

public class OBDQueryTest extends AbstractAnnotationTest {

	//String jdbcPath = "jdbc:postgresql://spitz.lbl.gov:5432/obd_phenotype";
	String jdbcPath = "jdbc:postgresql://localhost:5432/obd_phenotype_full";
	
	protected OBDQueryTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={""};
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		

	}

	
	public void testQuery() throws SQLException, ClassNotFoundException, IOException, DataAdapterException {
		OBDSQLDatabaseAdapterConfiguration wconfig = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		wconfig.getReadPaths().add(jdbcPath);
		OBDSQLDatabaseAdapter wadapter = new OBDSQLDatabaseAdapter();
		wadapter.setConfiguration(wconfig);
		wadapter.connect();

		session = new OBOSessionImpl();
		session.setDefaultNamespace(new Namespace("test"));
		// ion transport
		Collection<Annotation> annots = 
			wadapter.fetchAnnotationsByObject(session, new OBOClassImpl("GO:0006811"));
		System.err.println("N matching annots:"+annots.size());
		for (Annotation annot : annots)
			System.out.println("  match:"+annot);
		assertTrue(annots.size() > 0);
		
		int numAnnots = 
			wadapter.fetchAnnotationCountByObject(session, new OBOClassImpl("GO:0006811"));
		System.err.println("N matching annots (count q):"+numAnnots);
		assertTrue(numAnnots > 0);

		assertTrue(numAnnots == annots.size());
		
		float ic =
			wadapter.fetchAnnotationInformationContentByObject(session, new OBOClassImpl("GO:0006811"));
		System.err.println("IC:"+ic);
		assertTrue(ic > 0);
		
		writeTempOBOFile();
		
	}
	
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
			suite.addTest(new OBDQueryTest("testQuery"));
	}
}



