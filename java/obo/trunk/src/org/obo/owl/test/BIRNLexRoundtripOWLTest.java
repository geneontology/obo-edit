package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.datamodel.impl.BIRNLexMetadataMapping;
import org.obo.owl.datamodel.impl.SAO_1_2_OWLMetadataMapping;


public class BIRNLexRoundtripOWLTest extends AbstractOWLTest {

	public BIRNLexRoundtripOWLTest(String name) {
		super(name);
	}
	
	protected boolean isSourceOWL() {
		return true;
	}
	protected boolean isCombineOWLOntologies() {
		return true;
	}
	protected void addMappings(OWLAdapterConfiguration config) {
		MetadataMapping mapping = new BIRNLexMetadataMapping();
		config.addMetadataMapping(mapping);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://purl.org/nbirn/birnlex/ontology/birnlex.owl" };
		return Arrays.asList(files);
	}
	
	
	public void testHasLoaded() throws IOException, DataAdapterException {
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}


}



