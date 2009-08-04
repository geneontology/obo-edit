package org.obo.owl.test;
/**
 * 
 */
//package org.morphster.ontology.conversion;

import java.net.URI;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.dataadapter.OWLAdapter.OWLAdapterConfiguration;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;

/**
 * @author hamid
 * 
 */
public class Obo2Owl {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {
		
		Obo2Owl tool = new Obo2Owl();
		
		String src = args[0] + args[1];
		String dst = args[0] + args[2];
		String url = "http://purl.org/obo/all"; //args[3];
		
		tool.setURI(url);

		System.out.print("Reading: " + src);
		tool.readOBO(src);
		System.out.println(" ... done!");

		System.out.print("Writing: " + dst);
		tool.writeOWL(dst);
		System.out.println(" ... done!");
		
	}

	private OBOSession session; // OBO ontology

	private OWLAdapter adapter; // OWL adapter
	
	private OWLOntology ontology; // OWL ontology
	
	private URI uri; // URI for ontology

	public void readOBO(String file) throws Exception {

		// *** Taken from AbtractOBOTest
		OBOFileAdapter oboAdapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();

		config.getReadPaths().add(file);
		config.setAllowDangling(true);
		config.setBasicSave(false);
		config.setFailFast(false);
		
		session = oboAdapter.doOperation(OBOFileAdapter.READ_ONTOLOGY, config, null);
		// ***

		convert();
	}

	private void convert() throws DataAdapterException {

		adapter = new OWLAdapter();

		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		
		NCBOOboInOWLMetadataMapping oboInOWL = new NCBOOboInOWLMetadataMapping();
		oboInOWL.setSession(session);
		config.addMetadataMapping(oboInOWL);
		config.setAllowLossy(true);
		adapter.setConfiguration(config);
		
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		adapter.setManager(manager);
		
		ontology = adapter.obo2owl(session, uri);
	}

	public void writeOWL(String file) throws DataAdapterException {
		
		if (adapter == null) throw new NullPointerException("OWLAdapter does not exist!");
		
		OWLOntologyManager manager = adapter.getManager();

		URI physicalURI;
		if (!file.contains("file:"))
			file = "file:" + file;
		physicalURI = URI.create(file);
		
		try {
			manager.saveOntology(ontology, ((OWLAdapterConfiguration) adapter
					.getConfiguration()).getOntologyFormat(), physicalURI);
		} catch (Exception e) {
			e.printStackTrace();
			throw new DataAdapterException(e, "Write error");
		}
	}

	/**
	 * Taken from AbstractOBOTest.java and updated
	 * 
	 * @return
	 */
	protected String getResourcePath() {
		return "test_data";
	}
	
	public void setURI(String s) {
		uri = URI.create(s);
	}
}
