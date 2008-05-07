package org.obo.test;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.util.AnnotationUtil;

public class PostcompSyntaxFileTest extends AbstractAnnotationTest {

	public PostcompSyntaxFileTest(String name) {
		super(name);
	}

	public void setUp() throws Exception {
		
		// TODO: DRY - GOAnnotationFileTest
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		for (String f : getFilesToLoad()) {
			config.getReadPaths().add(
					getResourcePath()+"/" + f);
			System.err.println(f);
		}
		
		// intended for testing loading of annotation files, in which
		// ALL ontology terms may be danglers
		config.setAllowDangling(true);
		config.setBasicSave(false);
		session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);
	}
	
	public void testAnnot() throws IOException, DataAdapterException {
		testForName("ZFIN:ZDB-GENO-070219-2",
			"Df(LG03:sox8,sox9b)b971/b971;sox9a<sup>hi1134Tg/hi1134Tg</sup>");
		testInstanceType("ZFIN:ZDB-GENO-070219-2","SO:0001027");
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		for (Annotation annot : annots) {
			System.out.println(annot);
			LinkedObject ao = annot.getObject();
			System.out.println("obj name="+ao.getName());
		}
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
		suite.addTest(new PostcompSyntaxFileTest("testAnnot"));
	}


	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"sox9b_zfin.obo"};
		return Arrays.asList(files);
	}
}
