package org.oboedit.gui.components.ontologyGeneration.interfaces;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface OntologyClassInterface {

	public String getID();
	
	// LABEL
	public String getLabel();
	public String getLabel(String language);
	
	// DEFINITIONS
	//public String getDefinition();
	public String getDefinition(String language);
	//public List<String> getDefinitions();
	public List<String> getDefinitions(String language);
	
	// SYNONYMS, ABBREVIATIONS
	// public Set<String> getSynonyms();
	public Set<String> getSynonyms(String language);
	public List<String> getAbbreviations();
	
	// PARENTS, CHILDREN
	public List<? extends OntologyClassInterface> getChildren();
	public List<? extends OntologyClassInterface> getParents();
	public Map<String, String> getParentMap(String language);

}
