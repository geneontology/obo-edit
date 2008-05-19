package org.obo.test;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.obo.datamodel.OBOObject;
import org.obo.util.IDMapper;
import org.obo.util.IDMapper.SimpleAnnotation;

import org.apache.log4j.*;

public class IDMapperTest extends AbstractOBOTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDMapperTest.class);

	IDMapper mapper;
	public IDMapperTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "testfile.1.0.obo" };
		return Arrays.asList(files);
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
		// now try annot counts
		String inputPath = getResourcePath() + "/gene_assoc.test";
		Map<String, Collection<String>> e2ids = mapper.simpleAnnotationFileParse(inputPath);
		mapper.calcEntityCountByOboID(e2ids);
		Map<String, Integer> entityCountByOboIDMap = mapper.getEntityCountByOboIDMap();
		for (String id : entityCountByOboIDMap.keySet()) {
			OBOObject obj = (OBOObject)mapper.getSession().getObject(id);
			// TODO - handler class
			logger.info(entityCountByOboIDMap.get(id)+"\t"+id+"\t"+obj.getName());
		}
	}

}
