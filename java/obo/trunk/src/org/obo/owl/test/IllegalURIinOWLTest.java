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
import org.obo.datamodel.OBOClass;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;
import org.obo.owl.datamodel.impl.SimpleOWLMetadataMapping;

public class IllegalURIinOWLTest extends AbstractOWLTest {

	public IllegalURIinOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "regulation_of_transcription_xp.obo" };
		return Arrays.asList(files);
	}
	
	public boolean isAllowLossyWhenWritingOWL() {
		return true;
	}


	public void testHasLoaded() throws IOException, DataAdapterException {
		OBOClass cls = (OBOClass) session.getObjectFactory().createObject("a^b:c", OBOClass.OBO_CLASS, false);
		cls.setName("you shouldn't see this in lossy mode");
		session.addObject(cls);

		OBOClass cls2 = (OBOClass) session.getObjectFactory().createObject("abcdef:c^d", OBOClass.OBO_CLASS, false);
		//cls2.setName("you shouldn't see this in lossy mode");
		session.addObject(cls2);

		//File outFile = writeTempOWLFile(new NCBOOboInOWLMetadataMapping());
		File outFile = writeTempOWLFile(new SimpleOWLMetadataMapping());
		System.out.println("written "+outFile);
		//outFile = writeTempOWLFile();
		//outFile = writeTempOWLFile();
		readOWLFile(outFile);
		writeTempOBOFile();
	}

	

}



