package org.obo.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;

import org.apache.log4j.*;

/**
 * @author cjm
 *
 * Link namespaces currently not supported
 *
 */
@Deprecated
public class NamespaceTest extends AbstractOBOTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NamespaceTest.class);

	public NamespaceTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"nucleus.obo", "nucleus_xp.obo"};
		return Arrays.asList(files);
	}
	
	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testNamespace() throws Exception {
		this.testForNamespace("GO:0043226", "cellular_component");
		
		String id = "GO:0043229"; // intracellular organelle
		this.testForNamespace(id, "cellular_component");
		this.testForName(id, "intracellular organelle");
		testForGenus(id,"GO:0043226");
		IdentifiedObject io = session.getObject(id);
		for (Link link : 
			((LinkedObject)io).getParents()) {
			logger.info(link+" "+((OBORestriction)link).getCompletes()+" "+
					link.getNamespace());
			if (((OBORestriction)link).getCompletes())
				if (link.getType().equals(OBOProperty.IS_A))
					assertTrue(link.getNamespace().getID().equals("test"));
				else
					assertTrue(link.getNamespace().getID().equals("cc_xp"));
			else
				assertTrue(link.getNamespace().getID().equals("gene_ontology"));
		}
		
		io = session.getObject("GO:0005623");
		for (Link link : 
			((LinkedObject)io).getParents()) {
			logger.info(link+" "+((OBORestriction)link).getCompletes()+" "+
					link.getNamespace());
			if (link.getParent().equals("bfo:Object"))
				assertTrue(link.getNamespace().getID().contains("cc-bfo-mapping"));
			else
				assertTrue(link.getNamespace().getID().equals("gene_ontology"));		
							
		}		
		
	}


	public static void addTests(TestSuite suite) {
		suite.addTest(new NamespaceTest("testNamespace"));
	}

}

