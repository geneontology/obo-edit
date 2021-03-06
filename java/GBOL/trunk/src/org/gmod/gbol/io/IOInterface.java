package org.gmod.gbol.io;

import java.util.Collection;

import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.generated.AbstractFeature;

public interface IOInterface {
	
	public boolean write(AbstractSimpleObject simpleObject);
	public boolean write(Collection<AbstractSimpleObject> simpleObjects);
	public Collection<?extends AbstractFeature> readAll() throws Exception;
	
}