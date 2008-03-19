package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.datamodel.impl.MgedOntologyOWLMetadataMapping;

public class MORoundtripOWLTest extends AbstractOWLTest {

	public MORoundtripOWLTest(String name) {
		super(name);
	}
	
	protected boolean isSourceOWL() {
		return true;
	}
	protected boolean isAllowLossyWhenReadingOWL() {
		return true;
	}

	protected void addMappings(OWLAdapterConfiguration config) {
		MetadataMapping mapping = new MgedOntologyOWLMetadataMapping();
		config.addMetadataMapping(mapping);
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



