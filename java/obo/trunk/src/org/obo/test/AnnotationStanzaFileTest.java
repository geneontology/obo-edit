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

import org.apache.log4j.*;

public class AnnotationStanzaFileTest extends AbstractAnnotationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AnnotationStanzaFileTest.class);

	public AnnotationStanzaFileTest(String name) {
		super(name);
	}

	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"simple-annot-stanza-example.obo"};
		return Arrays.asList(files);
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

		/*
		 * instance-level checks
		 */
		
		Instance fred = (Instance)session.getObject("fred");
		logger.info(fred);
		
		// these must all be satisfied by the end of this part of the test
		boolean likesBread = false;
		boolean dislikesShuggy = false;
		boolean hasSSNo = false;
		// instance-level links such as "fred likes bread" are stored as normal links
		// this holds for both "DataType" links (ie those in obof with an xsd datatype)
		// and for "ObjectProperty" links (ie links between instances, or I-C links)
		for (Link link : fred.getParents()) {
			logger.info("  link: "+link+" :: "+link.getClass());
			if (link.getType().getID().equals("likes")) {
				if (link.getParent().getID().equals("bread")) {
					likesBread = true;
					assertTrue(link instanceof OBORestriction);
				}
			}
			if (link.getType().getID().equals("dislikes")) {
				assertTrue(link instanceof ValueLink);

				if (link.getParent().getID().equals("shuggy")) {
					dislikesShuggy = true;
					assertTrue(link instanceof ValueLink);
				}
			}

			if (link.getType().getID().equals("has_ss_no")) { 
				LinkedObject p = link.getParent();
				//logger.info(p+" :: "+p.getClass());
				logger.info(link.getClass());
				if (link instanceof ValueLink) {
					ValueLink ipv = (ValueLink)link;
					Value v = ipv.getValue();
					if (v instanceof DatatypeValue) {
						DatatypeValue dv = (DatatypeValue)v;
						logger.info(dv.getValue()+" :: "+ipv.getType()+ " :: "+
								v.getType());
						assertTrue(dv.getValue().equals("123-45-6789"));
						assertTrue(v.getType().getID().equals("xsd:string"));
						hasSSNo = true;
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
			// everything we want to know about fred is in fact in
			// normal links from fred, NOT propertyValues()
			// (even if property_value tags are used in obof)
			assertTrue(false);

			logger.info("  fred pv:"+pv);

			// prop and value are strings: NOT what I expect. CJM
			String prop = pv.getProperty();
			logger.info("  prop:"+prop);
			String v = pv.getValue();
			logger.info("  val:"+prop);

			if (prop.equals("has_ss_no")) {
				assertTrue(false);
			}

			if (prop.equals("likes"))
				assertTrue(false);
		}
		
		// check we found everything we need
		assertTrue(likesBread); 
		assertTrue(dislikesShuggy); 
		assertTrue(hasSSNo); 
	
		
		assertTrue(fred.getNamespace().getID().equals("test"));
		
		/*
		 * annotation stanza checks
		 */
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		logger.info("N annots:"+annots.size());
		assertTrue(annots.size() > 0);
		boolean annotCompareTestOk = false;
		for (Annotation annot : annots) {
			logger.info(annot.getNamespace()+" annot: "+annot+":: "+annot.getSubject()+" -"+annot.getRelationship()+"-> "+annot.getObject());  
			for (PropertyValue pv : annot.getPropertyValues()) {
				logger.info("  pv:"+pv);
			}
			if (annot.getSubject().getID().equals("biggles")) {
				if (annot.getObject().getID().equals("dread")) {
					for (Link link : annot.getParents()) {
						logger.info("BIGGLES:"+link);
						if (link.getType().getID().equals("holds_more_strongly_than")) {
							if (link.getParent().getID().equals("_:annot1"))
								annotCompareTestOk = true;
						}
					}
					
					// test annotation comparisons
					OBOProperty property = (OBOProperty)session.getObject("holds_more_strongly_than");
					IdentifiedObject annot2 = session.getObject("_:annot2");
					annot.addPropertyValue(property, annot2);
				}
			}
		}
		assertTrue(annotCompareTestOk);
		
		testForAnnotation("fred","bread");
		testForAnnotationAssignedBy("biggles","dread","GO");
		testForAnnotationPublication("biggles","dread","PMID:biggles_flies_again");

		for(IdentifiedObject io : session.getObjects()) {
			if (!io.isBuiltIn() && !(io instanceof Annotation)) {
				logger.info(" regular object "+io);
			}
		}

		/*
		 * class-level entity checks
		 */

		OBOObject bread = (OBOObject)session.getObject("bread");
		for (PropertyValue pv : bread.getPropertyValues()) {
			/*
			 * unfortunately it appears that property_value: tags in obof
			 * in Term stanzas are treated as unknown tags. This means the tag
			 * name (property_value) is the property and the actual P,V pair
			 * gets treated as an atomic unit separated by a space.
			 * This is not the desired behavior!!
			 * however, if we do put the actual P in property then we may break
			 * parsing of unknown tags if we are not careful. 
			 * 
			 * This is all a bit of a mess. I think we want to reserve
			 * getProperty and getValue for unknown tags (they don't bear much
			 * resemblance to property_value in obof), and I think we want to
			 * parse property_value for classes the same way we do for instances
			 * (see above). But if we do this we have to be very careful that these
			 * don't get mixed up with relationship: tags which have ALL-SOME semantics
			 * (contrast with property_value, which in a Term stanza means a property
			 * at the level of the class unit, ie class metadata)
			 */
			logger.info("  property:"+pv.getProperty()); // String
			logger.info("     value:"+pv.getValue()+"\n"); // String
			// TODO - property_value tags
		}
		// check newlines written out OK
		// this should really go in its own test; nothing to do with annotations per se
		bread.setDefinition("test\na b c d e f\nggg\ty\rfoo");


	}
	

}
