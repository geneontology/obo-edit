package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;

import org.apache.log4j.*;

public class OBO2OWLTest extends AbstractOWLTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2OWLTest.class);

	public OBO2OWLTest(String name) {
		super(name);
	}
	
	protected boolean isSourceOWL() {
		return false;
	}


	public Collection<String> getFilesToLoad() {
		String[] files = {  "camphor_catabolism.obo" };
		return Arrays.asList(files);
	}
	


}



