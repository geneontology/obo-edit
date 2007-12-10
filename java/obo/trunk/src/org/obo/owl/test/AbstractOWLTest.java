package org.obo.owl.test;

import java.io.File;
import java.io.IOException;

import org.bbop.dataadapter.DataAdapterException;
import org.coode.manchesterowlsyntax.ManchesterOWLSyntaxOntologyFormat;
import org.obo.dataadapter.OBOAdapter;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.datamodel.impl.AxiomAnnotationBasedOWLMetadataMapping;
import org.obo.owl.datamodel.impl.SimpleOWLMetadataMapping;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.test.AbstractOBOTest;
import org.semanticweb.owl.io.OWLFunctionalSyntaxOntologyFormat;

public abstract class AbstractOWLTest extends AbstractOBOTest {


	protected AbstractOWLTest(String name) {
		super(name);
	}

		
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// file -> session
		session = getSessionFromResources(getFilesToLoad());

		
	}

	public void testHasLoaded() throws IOException, DataAdapterException {
		File outFile = writeTempOWLFile();
		readOWLFile(outFile);
		writeTempOBOFile();
	}
	
	public File writeTempOWLFile() throws IOException, DataAdapterException {
		return writeTempOWLFile(new SimpleOWLMetadataMapping());
	}
	
	public File writeTempOWLFile(MetadataMapping mapping) throws IOException, DataAdapterException {
		
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		config.addMetadataMapping(mapping);
		//config.addMetadataMapping(new AxiomAnnotationBasedOWLMetadataMapping());
		//File outFile = File.createTempFile("foo", "bar");
		//config.setOntologyFormat(new OWLFunctionalSyntaxOntologyFormat());
		//config.setOntologyFormat(new ManchesterOWLSyntaxOntologyFormat());
		File outFile = new File("foo.owl");
		//outFile.deleteOnExit();
		config.setWritePath(outFile.getAbsolutePath());
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		return outFile;
	}
	
	public void readOWLFile(File file) throws DataAdapterException {
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		config.addMetadataMapping(new SimpleOWLMetadataMapping());
		config.getReadPaths().add(file.getAbsolutePath());
		session = adapter.doOperation(OWLAdapter.READ_ONTOLOGY, config,
				null);

	}

}



