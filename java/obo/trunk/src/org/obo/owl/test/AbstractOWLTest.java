package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;

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
	protected boolean isAllowLossyWhenReadingOWL() {
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
		readOWLFiles(names);
		return session;
	}


	public void testHasLoaded() throws IOException, DataAdapterException {
		if (isSourceOWL()) {
			File outFile = writeTempOBOFile();
			readOBOFile(outFile);
			writeTempOWLFile();	
		}
		else {
			File outFile = writeTempOWLFile();
			readOWLFile(outFile);
			writeTempOBOFile();	
		}
	}
	
	public File writeTempOWLFile() throws IOException, DataAdapterException {
		return writeTempOWLFile(new SimpleOWLMetadataMapping());
	}
	
	public File writeTempOWLFile(MetadataMapping mapping) throws IOException, DataAdapterException {
		
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		config.addMetadataMapping(mapping);
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
		readOWLFiles(Collections.singleton(file.toString()));
	}
	
	public void readOWLFiles(Collection<String> files) throws DataAdapterException {
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		addMappings(config);
		config.getReadPaths().addAll(files);
		config.setAllowLossy(isAllowLossyWhenReadingOWL());

		session = adapter.doOperation(OWLAdapter.READ_ONTOLOGY, config,
				null);

	}
	protected void addMappings(OWLAdapterConfiguration config) {
		config.addMetadataMapping(new SimpleOWLMetadataMapping());
	}

}



