package org.obo.test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.obo.datamodel.OBOObject;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.identifier.IDWarning;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.DefaultReasonerFactory;
import org.obo.util.IDMapper;

public class IDMapperTestWithReasoner extends AbstractOBOTest {

	IDMapper mapper;
	public IDMapperTestWithReasoner(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "regulation_of_transcription_xp.obo" };
		return Arrays.asList(files);
	}

	private void showObjs(Collection<OBOObject> objs) {
		for (OBOObject obj : objs) {
			System.out.println(obj.getID()+" "+obj.getName());
		}
	}
	
	public void testMapIDs() throws IOException {
		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		DefaultReasonerFactory rf = new DefaultReasonerFactory();
		ReasonedLinkDatabase reasoner = rf.createReasoner();
		mapper.setReasoner(reasoner);
		reasoner.recache();
		Collection<OBOObject> mappedObjs = mapper.mapIdentifierViaCategories("GO:0006355",false);
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 2);

		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_generic");
		mapper.setReasoner(reasoner);
		reasoner.recache();
		mapper.addPropertyToTraverse("regulates");
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0006357",false);
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 1);
		assertTrue(mappedObjs.iterator().next().equals(new OBOClassImpl("GO:0006350")));
	
		// regulation of transcription from RNA polymerase II promoter
		// involved in forebrain neuron fate commitment
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0021882",false);
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 1);
		assertTrue(mappedObjs.iterator().next().equals(new OBOClassImpl("GO:0006350")));

		// regulation of transcription from RNA polymerase II promoter
		// involved in forebrain neuron fate commitment
		mapper = new IDMapper();
		mapper.setSession(session);
		mapper.addCategory("goslim_plant");
		mapper.addCategory("gosubset_prok");
		mapper.setReasoner(reasoner);
		reasoner.recache();
		mapper.addPropertyToTraverse("part_of");
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0021882",false);
		showObjs(mappedObjs);
		assertTrue(mappedObjs.size() == 1);
		// cell differentiation
		assertTrue(mappedObjs.iterator().next().equals(new OBOClassImpl("GO:0030154")));
		mappedObjs = mapper.mapIdentifierViaCategories("GO:0021882",false);
		showObjs(mappedObjs);
		
		
		
		
	}

}
