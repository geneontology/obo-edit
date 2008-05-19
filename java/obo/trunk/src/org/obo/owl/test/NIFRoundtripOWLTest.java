package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.dataadapter.DataAdapterException;


import org.apache.log4j.*;

public class NIFRoundtripOWLTest extends AbstractOWLTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NIFRoundtripOWLTest.class);

	public NIFRoundtripOWLTest(String name) {
		super(name);
	}
	
	protected boolean isSourceOWL() {
		return true;
	}


	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://purl.org/nif/ontology/nif.owl" };
		return Arrays.asList(files);
	}
	
	
	public void testHasLoaded() throws IOException, DataAdapterException {
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}


}



