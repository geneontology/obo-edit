package org.gmod.gbol.io;

import java.util.Collection;

import org.gmod.gbol.simpleObject.AbstractSimpleObject;

public interface IOInterface {
	
	public boolean write(AbstractSimpleObject simpleObject);
	public boolean write(Collection<AbstractSimpleObject> simpleObjects);
	public Collection<AbstractSimpleObject> readAll() throws Exception;
	
}