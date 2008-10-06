package org.gmod.gbol.simpleObject;

import java.util.Iterator;

/** Abstract class for iterators used to retrieve AbstractSimpleObjects (mainly used for writing
 *  framework).
 * 
 * @author elee
 *
 */
public abstract class AbstractSimpleObjectIterator implements Iterator<AbstractSimpleObject> {

	protected AbstractSimpleObject current;

	/** Look at the current element in the iterator.  It should return the same
	 * AbstractSimpleObject object that would've been returned by a call to next() but 
	 * without incrementing the iterator.
	 * 
	 * @return AbstractSimpleObject that would've been returned by a call to next()
	 */
	abstract public AbstractSimpleObject peek();

	public void remove()
	{
	}
}
