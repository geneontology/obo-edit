package org.gmod.gbol.simpleObject;

import java.util.ArrayList;
import java.util.Collection;

import org.gmod.gbol.simpleObject.CV;

/*
 * Autogenerated extension of org.gmod.gbol.simpleObject.generated.CVTerm.
 * Add custom code to this file. 
 *
 * Author: Robert Bruggner, rbruggner@berkeleybop.org
 *
*/

public class CVTerm extends org.gmod.gbol.simpleObject.generated.AbstractCVTerm {

	private static final long serialVersionUID = 1L;

	public CVTerm(){
		super();
	}
	
	public CVTerm(String name,CV cv){
		this.setCv(cv);
        this.setName(name);
        this.setIsObsolete(0);
	}

	@Override
	public Collection<AbstractSimpleObject> getWriteObjects() {
		ArrayList<AbstractSimpleObject> writeObjects = new ArrayList<AbstractSimpleObject>();
		// Have to write yourself
		writeObjects.add(this);
		
		// Singletons
		writeObjects.addAll(this.getCv().getWriteObjects());
		writeObjects.addAll(this.getDbxref().getWriteObjects());

		// Specifically not traversing CVTermRelationships or CVTermPaths, since that 
		// could involve us in a loop or a very long traversal	
		
		return writeObjects;
	}
}
