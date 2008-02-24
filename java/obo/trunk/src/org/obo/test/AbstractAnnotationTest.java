package org.obo.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;

public abstract class AbstractAnnotationTest extends AbstractOBOTest {

	protected AbstractAnnotationTest(String name) {
		super(name);
	}


	public void testForAnnotation(String su, String ob) {
		assertTrue(getFirstAnnotation(su,ob) != null);
	}
	
	public void testForAnnotationAssignedBy(String su, String ob, String by)  {
		assertTrue(getFirstAnnotation(su,ob).getAssignedBy().getID().equals(by));
	}
	
	public void testForAnnotationPublication(String su, String ob, String pubId)  {
		assertTrue(getFirstAnnotationWithPublication(su,ob,pubId) != null);
	}
	
	public void testForAnnotationWithEvidenceCode(String su, String ob, String code)  {
		assertTrue(getFirstAnnotationWithEvidenceCode(su,ob,code) != null);
	}
	
	public Annotation getFirstAnnotationWithPublication(String su, String ob, String pubId)  {
		for (Annotation annot : getAllAnnotations(su, ob))
			for (IdentifiedObject pub : annot.getSources())
				if (pub.getID().equals(pubId))
					return annot;
		return null;
	}
	
	public Annotation getFirstAnnotationWithEvidenceCode(String su, String ob, String code)  {
		for (Annotation annot : getAllAnnotations(su, ob))
			for (IdentifiedObject ev : annot.getEvidence())
				if (((Instance)ev).getType().getID().equals(code))
					return annot;
		return null;
	}
	
	public Annotation getFirstAnnotation(String su, String ob) {
		IdentifiedObject io = session.getObject(su);
		if (io != null) {
			Collection<Annotation> annots = getAnnotationsForSubject(io);
			for (Annotation annot : annots) {
				if (ob.equals(annot.getObject().getID())) {
					return annot;
				}
			}
		}
		return null;
	}
	
	public Collection<Annotation> getAllAnnotations(String su, String ob) {
		IdentifiedObject io = session.getObject(su);
		HashSet<Annotation> matches = new HashSet<Annotation>();
		if (io != null) {
			Collection<Annotation> annots = getAnnotationsForSubject(io);
			for (Annotation annot : annots) {
				if (ob.equals(annot.getObject().getID())) {
					matches.add(annot);
				}
			}
		}
		return matches;
	}
		
	public Collection<Annotation> getAnnotationsForSubject(String id) {
		return getAnnotationsForSubject(session.getObject(id));
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
