package org.obo.owl.util;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.*;

public class IDSpaceRegistry {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDSpaceRegistry.class);

	private static IDSpaceRegistry instance;

	private Map<URI,String> uriToIDSpace;


	private IDSpaceRegistry() {
		uriToIDSpace = new HashMap<URI,String>();

		// defaults: these can be overridden at runtime
		registerMapping("http://www.ifomis.org/bfo/1.1#","bfo_1_1");
		registerMapping("http://www.ifomis.org/bfo/1.0#","bfo_1_0");
		registerMapping("http://purl.org/biotop/dev#","biotop_dev");
		registerMapping("http://ccdb.ucsd.edu/SAO/1.2#","sao");
		registerMapping("http://obi.sourceforge.net/ontology/OBI.owl#","obi");
		registerMapping("http://www.w3.org/2000/01/rdf-schema#","rdfs");
	}


	public static synchronized IDSpaceRegistry getInstance() {
		if (instance == null) {
			instance = new IDSpaceRegistry();
		}
		return instance;
	}


	public void clearuriToIDSpace() {
		uriToIDSpace.clear();
	}


	public Map<URI,String> getUriToIDSpace() {
		return uriToIDSpace;
	}

	public String getIDSpace(URI uriPrefix) {
		return uriToIDSpace.get(uriPrefix);
	}
	
	public Collection<URI> getUris() {
		return uriToIDSpace.keySet();
	}

	public void registerMapping(String prefix, String idSpace) {
		uriToIDSpace.put(URI.create(prefix), idSpace);
	}

	public void registerMapping(URI uriPrefix, String idSpace) {
		uriToIDSpace.put(uriPrefix, idSpace);
	}

	public void unregisterMapping(URI uriPrefix) {
		uriToIDSpace.remove(uriPrefix);
	}
}

