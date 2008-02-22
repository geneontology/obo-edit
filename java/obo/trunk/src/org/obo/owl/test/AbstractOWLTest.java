package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.datamodel.impl.SimpleOWLMetadataMapping;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.test.AbstractOBOTest;

public abstract class AbstractOWLTest extends AbstractOBOTest {


	protected boolean isSourceOWL() {
		return false;
	}
	protected boolean isAllowLossyWhenWritingOWL() {
		return false;
	}
	
	protected AbstractOWLTest(String name) {
		super(name);
	}

		
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// file -> session
		session = getSessionFromResources(getFilesToLoad());

		
	}
	
	@Override
	protected OBOSession getSessionFromResources(Collection<String> names)
	throws DataAdapterException {
		if (!isSourceOWL())
			return super.getSessionFromResources(names);
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		for (String f : names) {
			if (!f.contains("http:") && !f.contains("file:")) {
				f = "file:"+getResourcePath()+"/"+f;
				System.out.println(f);
			}
			config.getReadPaths().add(f);
		}
		config.setAllowLossy(true);
		session = adapter.doOperation(OWLAdapter.READ_ONTOLOGY, config,
				null);
		return session;
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
		if (isAllowLossyWhenWritingOWL()) {
			config.setAllowLossy(true);
		}
		File outFile = new File("file:///tmp/foo.owl");
		//outFile.deleteOnExit();
		//config.setWritePath(outFile.getAbsolutePath());
		config.setWritePath(outFile.toString());
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		return outFile;
	}
	
	public void readOWLFile(File file) throws DataAdapterException {
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		addMappings(config);
		config.getReadPaths().add(file.toString());
		session = adapter.doOperation(OWLAdapter.READ_ONTOLOGY, config,
				null);

	}
	protected void addMappings(OWLAdapterConfiguration config) {
		config.addMetadataMapping(new SimpleOWLMetadataMapping());
	}

}



