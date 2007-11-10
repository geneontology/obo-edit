package org.obo.test;

import java.io.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;


import junit.framework.*;

public abstract class AbstractAnnotationTest extends AbstractOBOTest {

	protected AbstractAnnotationTest(String name) {
		super(name);
	}


	public void testForAnnotation(String su, String ob) {
		IdentifiedObject io = session.getObject(su);
		boolean ok = false;
		if (io != null) {
			Collection<Annotation> annots = getAnnotationsForSubject(io);
			for (Annotation annot : annots) {
				if (ob.equals(annot.getObject().getID())) {
					ok = true;
				}
			}
		}
		assertTrue(ok);
	}
	
	public Collection<Annotation> getAnnotationsForSubject(IdentifiedObject su) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				if (su.equals(annot.getSubject())) {
					annots.add(annot);
				}
			}
		}
		return annots;
	}
	
	public File writeTempAssocFile() throws IOException, DataAdapterException {
		
		GOStyleAnnotationFileAdapter adapter = new GOStyleAnnotationFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		File outFile = File.createTempFile("foo", "bar");
		//outFile.deleteOnExit();
		config.setWritePath(outFile.getAbsolutePath());
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		return outFile;
	}


	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"gene_assoc.test"};
		return Arrays.asList(files);
	}
}
