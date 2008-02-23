package org.obo.util;

import java.util.Collection;
import java.util.Collections;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;

public class AdapterUtil {
	public enum AdapterFormat { OBO, OWL }
	
	public static OBOSession parseFile(String path) throws DataAdapterException {
		return parseFiles(Collections.singleton(path));
	}
	public static OBOSession parseFiles(Collection<String> paths) throws DataAdapterException {


		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		for (String path : paths) {
			config.getReadPaths().add(path);
		}
		config.setAllowDangling(true);
		config.setBasicSave(false);
		config.setFailFast(false);
		OBOSession 
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);
		return session;

	}
}
