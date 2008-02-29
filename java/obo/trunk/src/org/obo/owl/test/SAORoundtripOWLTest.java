package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.impl.OBIMetadataMapping;

public class SAORoundtripOWLTest extends AbstractOWLTest {

	protected SAORoundtripOWLTest(String name) {
		super(name);
	}
	
	protected boolean isSourceOWL() {
		return true;
	}
	protected void addMappings(OWLAdapterConfiguration config) {
		OBIMetadataMapping mapping = new OBIMetadataMapping();
		config.addMetadataMapping(mapping);
	}


	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://ccdb.ucsd.edu/SAO/1.2/SAO.owl" };
		return Arrays.asList(files);
	}
	
	
	public void testHasLoaded() throws IOException, DataAdapterException {
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}


}



