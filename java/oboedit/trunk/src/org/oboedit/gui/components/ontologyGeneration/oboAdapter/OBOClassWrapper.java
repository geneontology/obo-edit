package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyClassInterface;

public class OBOClassWrapper implements OntologyClassInterface {

	private LinkedObject linkedObject;
	
	private Set<String> synonyms;
	private List<OBOClassWrapper> children;
	private List<OBOClassWrapper> parents;
	private Map<String, String> parentMap;
	
	public OBOClassWrapper(LinkedObject linkedObject) {
		this.linkedObject = linkedObject;
	}
	
	public String getID() {
		return linkedObject.getID();
	}

	public String getLabel() {
		return linkedObject.getName();
	}

	public String getLabel(String language) {
		return linkedObject.getName();
	}

	public String getDefinition(String language) {
		if (linkedObject instanceof OBOClass)
			return ((OBOClass)linkedObject).getDefinition();
		return "";
	}

	public List<String> getDefinitions(String language) {
		return Collections.singletonList(this.getDefinition(language));
	}

	public Set<String> getSynonyms(String language) {
		if (synonyms == null) {
			if (linkedObject instanceof OBOClass) {
				synonyms = new HashSet<String>();
				
				Set<Synonym> linkedObjectSynonyms = ((OBOClass)linkedObject).getSynonyms();
				for (Synonym synonym : linkedObjectSynonyms) {
					synonyms.add(synonym.getText());
				}
			}
			synonyms = Collections.emptySet();
		}

		return synonyms;
	}

	public List<String> getAbbreviations() {
		return Collections.emptyList();
	}

	public List<? extends OntologyClassInterface> getChildren() {
		if (children == null) {
			children = new ArrayList<OBOClassWrapper>(); 

			Collection<Link> linkedObjectChildren = linkedObject.getChildren();
			for (Link childLink : linkedObjectChildren) {
				LinkedObject child = childLink.getChild();
				if (child != null)
					children.add(new OBOClassWrapper(child));
			}
		}
		
		return children;
	}

	public List<? extends OntologyClassInterface> getParents() {
		if (parents == null) {
			parents = new ArrayList<OBOClassWrapper>(); 

			Collection<Link> linkedObjectParents = linkedObject.getParents();
			for (Link parentLink : linkedObjectParents) {
				LinkedObject parent = parentLink.getParent();
				if (parent != null)
					parents.add(new OBOClassWrapper(parent));
			}
		}
		
		return parents;
	}

	public Map<String, String> getParentMap(String language) {
		if (parentMap == null) {
			parentMap = new HashMap<String, String>();
			
			Collection<Link> parents = linkedObject.getParents();
			for (Link parentLink : parents) {
				LinkedObject parent = parentLink.getParent();
				if (parent != null)
					parentMap.put(parent.getID(), ((LinkedObject) parent).getName());
			}
		}
		
		return parentMap;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((linkedObject == null) ? 0 : linkedObject.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		OBOClassWrapper other = (OBOClassWrapper) obj;
		if (linkedObject == null) {
			if (other.linkedObject != null)
				return false;
		} else if (!linkedObject.equals(other.linkedObject))
			return false;
		return true;
	}

}
