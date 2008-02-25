package org.obo.test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.obo.datamodel.OBOObject;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.identifier.IDWarning;
import org.obo.util.IDMapper;

public class IDMapperTestWithIDLifecycle extends AbstractOBOTest {

	IDMapper mapper;
	public IDMapperTestWithIDLifecycle(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "cellular_component.obo" };
		return Arrays.asList(files);
	}

	private void showObjs(Collection<OBOObject> objs) {
		for (OBOObject obj : objs) {
			System.out.println(obj.getID()+" "+obj.getName());
		}
	}
	
	private void showWarnings(Collection<IDWarning> warnings) {
		for (IDWarning warning: warnings)
			System.out.println("warning: "+warning);
	}

	public void testMapIDs() throws IOException {
		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		Collection<OBOObject> mappedObjs = mapper.mapIdentifierViaCategories("GO:0005921",false);
		assertTrue(mappedObjs.size() == 1);
		assertTrue(mappedObjs.iterator().next().getName().equals("plasma membrane"));
		
		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		// now try an obsolete that has 2 replaced_bys
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0005660",false);
		showWarnings(mapper.getWarnings());
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 3);
		assertTrue(mappedObjs.contains(new OBOClassImpl("GO:0043234")));
		
		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		mapper.setAutoReplaceConsiderTags(true);
		// now try an obsolete with 3 considers and 1 replaced_by,
		// in which the replaced_by is in another ontology
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0005645",false);
		showWarnings(mapper.getWarnings());
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 4);
		
		// repeat the above test...
		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		mapper.setAutoReplaceConsiderTags(false); // ...but don't use considers
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0005645",false);
		showWarnings(mapper.getWarnings());
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 0);
	
		
	}

}
