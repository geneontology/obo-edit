package org.obo.owl.test;

import java.util.Arrays;
import java.util.Collection;

import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.impl.OBIMetadataMapping;

public class OBIRoundtripOWLTest extends AbstractOWLTest {

	protected boolean isSourceOWL() {
		return true;
	}
	protected boolean isAllowLossyWhenWritingOWL() {
		return true;
	}
	protected void addMappings(OWLAdapterConfiguration config) {
		OBIMetadataMapping mapping = mapping = new OBIMetadataMapping();
		config.addMetadataMapping(mapping);
	}

	public OBIRoundtripOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "obi.owl" };
		return Arrays.asList(files);
	}
	
		

}



