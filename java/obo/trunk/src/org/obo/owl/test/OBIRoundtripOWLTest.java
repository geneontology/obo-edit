package org.obo.owl.test;

import java.util.Arrays;
import java.util.Collection;

import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.impl.OBIMetadataMapping;

public class OBIRoundtripOWLTest extends AbstractOWLTest {

	@Override
	protected boolean isSourceOWL() {
		return true;
	}
	@Override
	protected boolean isAllowLossyWhenWritingOWL() {
		return true;
	}
	@Override
	protected boolean isAllowLossyWhenReadingOWL() {
		return true;
	}
	
	@Override
	public boolean getAllowDangling() {
		return true;
	}

	
	@Override
	protected void addMappings(OWLAdapterConfiguration config) {
		OBIMetadataMapping mapping = new OBIMetadataMapping();
		config.addMetadataMapping(mapping);
	}

	public OBIRoundtripOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://obi.svn.sourceforge.net/viewvc/*checkout*/obi/ontology/trunk/OBI.owl" };
		return Arrays.asList(files);
	}
	
		

}



