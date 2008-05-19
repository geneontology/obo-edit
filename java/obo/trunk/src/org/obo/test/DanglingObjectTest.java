package org.obo.test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.annotation.datamodel.Annotation;
import org.obo.datamodel.DanglingObject;
import org.obo.datamodel.DatatypeValue;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.Value;
import org.obo.datamodel.ValueLink;
import org.obo.util.AdapterUtil;
import org.obo.util.AnnotationUtil;

import org.apache.log4j.*;

public class DanglingObjectTest extends AbstractAnnotationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DanglingObjectTest.class);

	public DanglingObjectTest(String name) {
		super(name);
	}

	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"dangling_object_test.obo"};
		return Arrays.asList(files);
	}

	public void testDangling() throws IOException, DataAdapterException {

		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			logger.info(io.getID()+" :: "+io.getName());
		}
		
		OBOClass clsA = (OBOClass) session.getObject("clsA");
		checkObject(clsA,false,false);
		OBOClass clsAcln = (OBOClass) clsA.clone();
		checkObject(clsAcln,false,false);
		
		OBOClass clsB = (OBOClass) session.getObject("clsB");
		checkObject(clsB,false,true);
		OBOClass clsBcln = (OBOClass) clsB.clone();
		checkObject(clsBcln,false,true);
			
		Instance inst = (Instance) session.getObject("fred");
		checkObject(inst,false,false);		
		Instance instcln = (Instance) inst.clone();
		checkObject(instcln,false,false);		
		
		OBOProperty prop = (OBOProperty) session.getObject("foo");
		checkObject(prop,false,false);		
		OBOProperty propcln = (OBOProperty) prop.clone();
		checkObject(propcln,false,false);		
		
		
		//superfoo gets parsed as a dangling object, but not a dangling property
		OBOObject sprop = (OBOObject) session.getObject("superfoo");
		checkObject(sprop,false,true);		
		
		OBOSession s2 = AdapterUtil.parseFile(getResourcePath() + "/dangling_object_test-append.obo");
		clsA = (OBOClass) s2.getObject("clsA");
		checkObject(clsA,true,false);
		clsAcln = (OBOClass) clsA.clone();
		checkObject(clsAcln,true,false);
		assertTrue(clsAcln.getName().equals("a"));
		clsB = (OBOClass) s2.getObject("clsB");
		checkObject(clsB,false,false);
		OBOClass clsC = (OBOClass) s2.getObject("clsC");
		checkObject(clsC,false,true);
		
		/*
		 * importing a session:
		 * we would like this to merge objects; for example if o1 says clsA.name=null and o2 says clsA.name=a, we
		 * would like the non-null value to take precedence. Unfortunately, this doesn't happen as yet,
		 * so these tests are commented
		 */
		session.importSession(s2, true);
		clsA = (OBOClass) session.getObject("clsA");
		//checkObject(clsA,true,false); // DESIRED - clsA gets named by o2
		checkObject(clsA,false,false); // ACTUAL - clsA retains null name

		clsB = (OBOClass) session.getObject("clsB");
		checkObject(clsB,false,false); // clsB loses its dangling status, as has a term stanza in o2

		prop = (OBOProperty) session.getObject("foo");
		//checkObject(prop,false,false);		 // DESIRED - gets named by o2
		OBOProperty superfoo = (OBOProperty) session.getObject("superfoo");
		checkObject(superfoo,false,false);		

		// sprop is left unchanged. this is a slightly odd one. we now have two distinct objects
		checkObject(sprop,false,true);		

		
	}
	
	private void checkObject(OBOObject obj, boolean named, boolean dangling) {
		assertTrue(obj != null);
		logger.info("obj="+obj);
		assertTrue(obj.getName() == null ? !named : named);
		assertTrue(obj instanceof DanglingObject ? dangling : !dangling);
		
	}
	
}
