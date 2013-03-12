package org.obo.test;

import java.util.Collection;
import java.util.Collections;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.OBOClass;
import org.obo.util.TermUtil;

public class MarkAsInferredTest extends AbstractOBOTest{

	public MarkAsInferredTest() {
		super(MarkAsInferredTest.class.getSimpleName());
	}

	@Override
	public Collection<String> getFilesToLoad() {
		return Collections.singleton("marked_as_inferred.obo");
	}

	
	public void testIsMarkedAsInferred() throws Exception {
		IdentifiedObject object1 = linkDatabase.getObject("TEST:02");
		assertTrue(object1 instanceof OBOClass);
		
		OBOClass cls1 = (OBOClass) object1;
		Collection<Link> parents1 = cls1.getParents();
		assertEquals(1, parents1.size());
		
		Link link1 = parents1.iterator().next();
		
		assertTrue(TermUtil.isMarkedAsInferred(link1));
		
		IdentifiedObject object2 = linkDatabase.getObject("TEST:03");
		assertTrue(object2 instanceof OBOClass);
		
		OBOClass cls2 = (OBOClass) object2;
		Collection<Link> parents2 = cls2.getParents();
		assertEquals(1, parents2.size());
		
		Link link2 = parents2.iterator().next();
		
		assertFalse(TermUtil.isMarkedAsInferred(link2));
			
	}
}
