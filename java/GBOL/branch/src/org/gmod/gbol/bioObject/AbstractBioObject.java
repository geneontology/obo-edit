package org.gmod.gbol.bioObject;

import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;

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
}
