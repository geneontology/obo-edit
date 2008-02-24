package org.obo.test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.obo.datamodel.OBOObject;
import org.obo.util.IDMapper;


public class IDMapperTest extends AbstractOBOTest {

	
	public IDMapperTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "testfile.1.0.obo" };
		return Arrays.asList(files);
	}



	public void testMapIDs() throws IOException {
		IDMapper mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		Collection<OBOObject> mappedObjs = mapper.mapIdentifierViaCategories("GO:0005921");
		assertTrue(mappedObjs.size() == 1);
		assertTrue(mappedObjs.iterator().next().getName().equals("plasma membrane"));
		mapper.mapIDsInFile(getResourcePath() + "/gene_assoc.test");
	}


}
