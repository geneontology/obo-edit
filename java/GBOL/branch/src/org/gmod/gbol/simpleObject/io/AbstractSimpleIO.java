package org.gmod.gbol.simpleObject.io;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.gmod.gbol.simpleObject.AbstractSimpleObject;

public abstract class AbstractSimpleIO implements SimpleObjectIOInterface {

	public boolean write(Collection<AbstractSimpleObject> simpleObjects)
	{
		List<AbstractSimpleObject> flatSimpleObjects = new ArrayList<AbstractSimpleObject>();
		for (AbstractSimpleObject o : simpleObjects) {
			flatSimpleObjects.addAll(o.getWriteObjects());
		}
		return writeFlattenedObjects(flatSimpleObjects);
	}
	
	abstract protected boolean writeFlattenedObjects(Collection<AbstractSimpleObject> flatSimpleObjects);
}
