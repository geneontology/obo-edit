package org.gmod.gbol.simpleObject;

import java.util.ArrayList;
import java.util.Collection;

/*
 * Autogenerated extension of org.gmod.gbol.simpleObject.generated.FeatureProperty.
 * Add custom code to this file. 
 *
 * Author: Robert Bruggner, rbruggner@berkeleybop.org
 *
*/

public class FeatureProperty extends org.gmod.gbol.simpleObject.generated.AbstractFeatureProperty {

	private static final long serialVersionUID = 1L;

	public FeatureProperty(){
		super();
	}

	@Override
	public Collection<AbstractSimpleObject> getWriteObjects() {
		ArrayList<AbstractSimpleObject> writeObjects = new ArrayList<AbstractSimpleObject>();
		// Have to write yourself
		writeObjects.add(this);
		
		// Singletons
		writeObjects.addAll(this.getType().getWriteObjects());
		
		// Multiples
		for (FeaturePropertyPublication fpp : this.getFeaturePropertyPublications())
			writeObjects.addAll(fpp.getWriteObjects());
		
		return writeObjects;
	}
}
