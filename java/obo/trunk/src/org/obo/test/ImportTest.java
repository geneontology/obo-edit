package org.obo.test;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;

import org.bbop.dataadapter.CancelledAdapterException;
import org.obo.dataadapter.OBOConstants.TagMapping;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.dataadapter.OBOSerializer;
import org.obo.dataadapter.OBO_1_2_Serializer;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PropertyValue;
import org.obo.filters.Filter;

public class ImportTest extends AbstractOBOTest {

	public ImportTest(String name) {
		super(name);
	}

	@Override
	public Collection<String> getFilesToLoad() {
		return Collections.singleton("import_test/a.obo");
	}

	
	public void testLoad() {
		OBOClass one = (OBOClass) session.getObject("A:0001");
		assertNotNull(one);
		
		OBOClass two = (OBOClass) session.getObject("B:0002");
		assertNotNull(two);
		
		OBOClass three = (OBOClass) session.getObject("C:0003");
		assertNotNull(three);
		
		Collection<PropertyValue> propertyValues = session.getPropertyValues();
		assertEquals(2, propertyValues.size());
		for (PropertyValue propertyValue : propertyValues) {
			String p = propertyValue.getProperty();
			String v = propertyValue.getValue();
			if ("import".equals(p)) {
				assertEquals("http://purl.obolibrary.org/obo/foo.owl", v);
			}
			else if ("ontology".equals(p)) {
				assertEquals("A", v);
			}
			else {
				fail("Unexpected property"+p);
			}
		}
		
		assertEquals("namespace_a", session.getDefaultNamespace().getID());
	}
	
	public void testWrite() throws Exception {
		final OBOSerializer serializer = new OBO_1_2_Serializer();
		OBOSerializationEngine engine = new OBOSerializationEngine() {

			@Override
			public void writeFile(OBOSession session, Filter objectFilter,
					Filter linkFilter, HashSet<TagMapping> tagFilter,
					OBOSerializer serializer, PrintStream stream,
					FilteredPath filteredPath) throws IOException,
					CancelledAdapterException
			{
				setSerializer(serializer);
				super.writeFile(session, objectFilter, linkFilter, tagFilter, serializer,
						stream, filteredPath);
			}
			
		};
		Filter objectFilter = null;
		Filter linkFilter = null;
		HashSet<TagMapping> tagFilter = null;
		serializer.setEngine(engine);
		PrintStream stream = System.out;
		FilteredPath filteredPath = new FilteredPath();
		filteredPath.setPath("foo");
		engine.writeFile(session, objectFilter, linkFilter, tagFilter, serializer , stream, filteredPath);
	}
}
