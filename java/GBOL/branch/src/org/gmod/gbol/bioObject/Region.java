package org.gmod.gbol.bioObject;

import java.util.Collection;

import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;

/** Wrapper class representing a generic region.
 * 
 * @author elee
 *
 */

public class Region extends AbstractBioObject {

	protected Feature feature;

	/** Constructor.
	 * 
	 * @param feature - Feature object that this class wraps
	 * @param conf - Configuration containing mapping information
	 */
	public Region(Feature feature, BioObjectConfiguration conf)
	{
		super(conf);
		this.feature = feature;
	}

	/** Get all feature locations associated with this region.
	 * 
	 * @return Collection of feature locations associated with this region
	 */
	public Collection<FeatureLocation> getFeatureLocations()
	{
		return feature.getFeatureLocations();
	}
	
	/** Get the name of this region.
	 * 
	 * @return Name of this region
	 */
	public String getName()
	{
		return feature.getName();
	}
}
