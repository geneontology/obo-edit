package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;

public class MORoundtripOWLTest extends AbstractOWLTest {

	public MORoundtripOWLTest(String name) {
		super(name);
	}
	
	protected boolean isSourceOWL() {
		return true;
	}
	protected void addMappings(OWLAdapterConfiguration config) {
//		MetadataMapping mapping = new SAO_1_2_OWLMetadataMapping();
//		config.addMetadataMapping(mapping);
	}


	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://mged.sourceforge.net/ontologies/MGEDOntology.owl" };
		return Arrays.asList(files);
	}
	
	
	public void testHasLoaded() throws IOException, DataAdapterException {
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}


}



