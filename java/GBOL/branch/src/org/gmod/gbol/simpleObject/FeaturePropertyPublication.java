package org.gmod.gbol.simpleObject;

import java.util.ArrayList;
import java.util.Collection;

/*
 * Autogenerated extension of org.gmod.gbol.simpleObject.generated.FeaturePropertyPublication.
 * Add custom code to this file. 
 *
 * Author: Robert Bruggner, rbruggner@berkeleybop.org
 *
*/

public class FeaturePropertyPublication extends org.gmod.gbol.simpleObject.generated.AbstractFeaturePropertyPublication {

	private static final long serialVersionUID = 1L;

	public FeaturePropertyPublication(){
		super();
	}

	@Override
	public Collection<AbstractSimpleObject> getWriteObjects() {
		ArrayList<AbstractSimpleObject> writeObjects = new ArrayList<AbstractSimpleObject>();
		// Have to write yourself
		writeObjects.add(this);
		
		// Singletons
		writeObjects.addAll(this.getPublication().getWriteObjects());
		
		return writeObjects;
	}
}
