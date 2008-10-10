package org.gmod.gbol.simpleObject;

import java.util.Collection;

public abstract class AbstractSimpleObject {

	abstract public Collection<AbstractSimpleObject> getWriteObjects();

	abstract public AbstractSimpleObjectIterator getWriteableObjects();

	abstract public AbstractSimpleObject generateClone();
}