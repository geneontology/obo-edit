package org.obo.owl.test;
/**
 * 
 */
//package org.morphster.ontology.conversion;

import java.net.URI;
import java.util.Set;

import org.bbop.dataadapter.DataAdapterException;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.OBOSession;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyManager;

/**
 * @author hamid
 * 
 */
public class Owl2Obo {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {

		Owl2Obo tool = new Owl2Obo();

		String src = args[0] + args[1];
		String dst = args[0] + args[2];
		String url = "http://purl.org/obo/all"; //args[3];

		System.out.print("Reading: " + src);
		tool.readOWL(src);
		System.out.println(" ... done!");

		System.out.print("Writing: " + dst);
		tool.writeOBO(dst);
		System.out.println(" ... done!");

	}

	private OBOSession session; // OBO ontology

	private OWLAdapter adapter; // OWL adapter

	public void readOWL(String file) throws Exception {

		adapter = new OWLAdapter();

		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		config.getReadPaths().add(file);
		config.setAllowDangling(true);
		config.setBasicSave(false);

		config.addMetadataMapping(new NCBOOboInOWLMetadataMapping());

		session = (OBOSession) adapter.doOperation(OWLAdapter.READ_ONTOLOGY, config, null);

	}

	public void writeOBO(String file) throws DataAdapterException {
		
		OBOFileAdapter wadapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		
		config.setBasicSave(true);
		config.setAllowDangling(true);
		config.setSerializer("OBO_1_2");
		config.setWritePath(file);
		
		wadapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		
	}
		
}
