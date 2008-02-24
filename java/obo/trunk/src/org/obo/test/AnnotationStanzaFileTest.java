package org.obo.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.annotation.datamodel.Annotation;
import org.obo.datamodel.DatatypeValue;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.Value;
import org.obo.datamodel.ValueLink;
import org.obo.util.AnnotationUtil;

public class AnnotationStanzaFileTest extends AbstractAnnotationTest {

	public AnnotationStanzaFileTest(String name) {
		super(name);
	}

	public void testAnnot() throws IOException, DataAdapterException {
		/*
		 * Instance testing:
		 * 
		 * find an instance (fred), check fred's relations to other entities.
		 * These are an instance-instance like "fred likes bread"
		 * and instance-datatype "fred has_social_security_no xxx"
		 */
		testAnnot(session);
		File file = writeTempOBOFile();
		session = getSessionFromResources(Collections.singleton(file.toString()));
		testAnnot(session);
	}
	private void testAnnot(OBOSession session) throws IOException, DataAdapterException {
		Instance fred = (Instance)session.getObject("fred");
		System.out.println(fred);
		boolean likesBread = false;
		// instance-level links such as "fred likes bread" are stored as normal links
		for (Link link : fred.getParents()) {
			System.out.println("  link: "+link+" :: "+link.getClass());
			if (link.getType().getID().equals("likes")) {
				if (link.getParent().getID().equals("bread")) {
					likesBread = true;
					assertTrue(link instanceof OBORestriction);
				}
			}
			if (link.getType().getID().equals("dislikes")) {
				assertTrue(link instanceof ValueLink);

				if (link.getParent().getID().equals("shuggy")) {
					assertTrue(link instanceof ValueLink);
				}
			}

			if (link.getType().getID().equals("has_ss_no")) { 
				LinkedObject p = link.getParent();
				//System.out.println(p+" :: "+p.getClass());
				System.out.println(link.getClass());
				if (link instanceof ValueLink) {
					ValueLink ipv = (ValueLink)link;
					Value v = ipv.getValue();
					if (v instanceof DatatypeValue) {
						DatatypeValue dv = (DatatypeValue)v;
						System.out.println(dv.getValue()+" :: "+ipv.getType()+ " :: "+
								v.getType());
						assertTrue(dv.getValue().equals("123-45-6789"));
						assertTrue(v.getType().getID().equals("xsd:string"));
					}
					else {
						assertTrue(false);
					}

				}
				else {
					assertTrue(false);
				}
			}

		}
		for (PropertyValue pv : fred.getPropertyValues()) {
			// THIS IS NEVER REACHED!!!

			System.out.println("  fred pv:"+pv);

			// prop and value are strings: NOT what I expect. CJM
			String prop = pv.getProperty();
			System.out.println("  prop:"+prop);
			String v = pv.getValue();
			System.out.println("  val:"+prop);

			if (prop.equals("has_ss_no")) {
				assertTrue(false);
			}

			if (prop.equals("likes"))
				assertTrue(false);
		}
		assertTrue(likesBread); 
		assertTrue(fred.getNamespace().getID().equals("test"));
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		boolean annotCompareTestOk = false;
		for (Annotation annot : annots) {
			System.out.println(annot.getNamespace()+" annot: "+annot+":: "+annot.getSubject()+" -"+annot.getRelationship()+"-> "+annot.getObject());  
			for (PropertyValue pv : annot.getPropertyValues()) {
				System.out.println("  pv:"+pv);
			}
			if (annot.getSubject().getID().equals("biggles")) {
				if (annot.getObject().getID().equals("dread")) {
					for (Link link : annot.getParents()) {
						System.out.println("BIGGLES:"+link);
						if (link.getType().getID().equals("holds_more_strongly_than")) {
							if (link.getParent().getID().equals("_:annot1"))
								annotCompareTestOk = true;
						}
					}
					OBOProperty property = (OBOProperty)session.getObject("holds_more_strongly_than");
					IdentifiedObject annot2 = session.getObject("_:annot2");
					annot.addPropertyValue(property, annot2);
				}
			}
		}
		assertTrue(annotCompareTestOk);
		for(IdentifiedObject io : session.getObjects()) {
			if (!io.isBuiltIn() && !(io instanceof Annotation)) {
				System.out.println(" regular object "+io);
			}
		}

		OBOObject bread = (OBOObject)session.getObject("bread");
		for (PropertyValue pv : bread.getPropertyValues()) {
			System.out.println("  pv:"+pv);
		}
		// check newlines written out OK
		// this should really go in its own test; nothing to do with annotations per se
		bread.setDefinition("test\na b c d e f\nggg\ty\rfoo");

		testForAnnotation("fred","bread");
		testForAnnotationAssignedBy("biggles","dread","GO");
		testForAnnotationPublication("biggles","dread","PMID:biggles_flies_again");

	}
	

	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"simple-annot-stanza-example.obo"};
		return Arrays.asList(files);
	}
}
