package org.obo.reasoner;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;

/**
 * 
 * <code>
 * ReasonerRegsistry rr = ReasonerRegsistry.getInstance();
 * </code>
 * @author cjm
 *
 */
public class ReasonerRegistry {

	private static ReasonerRegistry instance;

	private Map<String,ReasonerFactory> factoryMap;
	private ReasonerFactory defaultReasonerFactory;
        private String defaultReasonerName;

	private ReasonerRegistry() {
		factoryMap = new HashMap<String,ReasonerFactory>();
		// defaults: these can be overridden at runtime
		// Using strings as identifiers for reasoners seems a bit dangerous--shouldn't they have
		// IDs or something?
		defaultReasonerFactory = new ForwardChainingReasonerFactory();  // For now
		defaultReasonerName = "ForwardChainingReasoner (old, faster)";  // For now
		registerReasoner("ForwardChainingReasoner (old, faster)", defaultReasonerFactory);
		registerReasoner("LinkPileReasoner (new, slower)", new LinkPileReasonerFactory());
	}


	/**
	 * 
	 * @return a reasoner registry which in shared throughout the session
	 * (i.e. it is static)
	 */
	public static synchronized ReasonerRegistry getInstance() {
		if (instance == null) {
			instance = new ReasonerRegistry();
		}
		return instance;
	}


	public void clearFactoryMap() {
		factoryMap.clear();
	}

	/**
	 * @return the names of all reasoner types
	 */
	public Collection<String> getRegisteredNames() {
		return factoryMap.keySet();
	}
	
	public Collection<ReasonerFactory> getRegisteredFactories() {
		return factoryMap.values();
	}

	/**
	 * @param name  - the name of a type of reasoner
	 * @return a factory that will create a reasoner of that type
	 */
	public ReasonerFactory lookupFactory(String name) {
		return factoryMap.get(name);
	}
	
	public void registerReasoner(String name, ReasonerFactory factory) {
		factoryMap.put(name, factory);
	}

	public void unregisterMapping(String name) {
		factoryMap.remove(name);
	}

    public ReasonerFactory getDefaultReasonerFactory() {
	return defaultReasonerFactory;
    }
    public String getDefaultReasonerName() {
	return defaultReasonerName;
    }


}
