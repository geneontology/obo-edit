package org.gmod.gbol.bioObject;

import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.simpleObject.AbstractSimpleObjectIterator;

/** Abstract wrapper class from which all Bio objects inherit from.
 * 
 * @author elee
 *
 */

public abstract class AbstractBioObject {

	protected BioObjectConfiguration conf;
	
	/** Constructor.
	 * 
	 * @param conf - Configuration containing mapping information
	 */
	public AbstractBioObject(BioObjectConfiguration conf)
	{
		this.conf = conf;
	}
	
	/** Returns an iterator that contains the high level AbstractSimpleObjects corresponding to the
	 *  underlying AbstractSimpleObject.
	 *  
	 * @param conf - Configuration containing mapping information
	 * @return AbstractSimpleObjectIterator for iterating through high level AbstractSimpleObjects to be written
	 */
	public abstract AbstractSimpleObjectIterator getWriteableSimpleObjects(BioObjectConfiguration conf);
	
}
