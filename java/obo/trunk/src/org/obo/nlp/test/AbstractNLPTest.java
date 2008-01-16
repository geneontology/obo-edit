package org.obo.nlp.test;

import java.io.File;
import java.io.IOException;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.nlp.SemanticParser;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.datamodel.impl.SimpleOWLMetadataMapping;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.test.AbstractOBOTest;

public abstract class AbstractNLPTest extends AbstractOBOTest {

	protected SemanticParser semanticParser;
	protected AbstractNLPTest(String name) {
		super(name);
	}
		
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// file -> session
		session = getSessionFromResources(getFilesToLoad());

		
	}


	public void testForIsA(ReasonedLinkDatabase ldb, String childID, String parentID)  {
		LinkedObject child = (LinkedObject) session.getObject(childID);
		LinkedObject parent = (LinkedObject) session.getObject(parentID);
		System.out.println(ldb+" testing "+child+" - "+parent);
		assertTrue(ldb.hasRelationship(child, OBOProperty.IS_A, parent) != null);
		//Collection<PathCapable> path = ReasonerUtil.getShortestExplanationPath(reasonedDB,child, OBOProperty.IS_A, parent);
		//System.out.println(path+" pathlen="+path.size());
	}


}



