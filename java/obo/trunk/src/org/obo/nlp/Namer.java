package org.obo.nlp;

import java.util.Collection;

import org.obo.datamodel.LinkedObject;

public interface Namer {

	public Collection<String> constructNames(LinkedObject lo);
	public Collection<String> constructTextDefs(LinkedObject lo);
	
}
