package org.gmod.gbol.bioObject;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.bioObject.conf.BioObjectConfigurationException;
import org.gmod.gbol.bioObject.util.BioObjectUtil;
import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.AbstractSimpleObjectIterator;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;
import org.gmod.gbol.simpleObject.FeatureRelationship;

/** Abstract class for all Bio objects that wrap a Feature object.
 * 
 * @author elee
 *
 */
public abstract class AbstractBioFeature extends AbstractBioObject {

	protected Feature feature;
	
	/** Constructor.
	 * 
	 * @param feature - Feature object that this object will wrap
	 * @param conf - Configuration containing mapping information
	 */
	public AbstractBioFeature(Feature feature, BioObjectConfiguration conf)
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
	
	public SimpleObjectIterator getWriteableSimpleObjects(BioObjectConfiguration c)
	{
		return new SimpleObjectIterator(this, c);
	}

	protected Iterator<AbstractBioFeatureRelationship> getBioFeatureRelationships()
	{
		return new BioFeatureRelationshipIterator(this, conf);
	}

	protected Feature translateSimpleObjectType(BioObjectConfiguration c)
	{
		Feature clone = new Feature(feature);
		// no cvterm found for this type, use default
		if (c.getClassForCVTerm(feature.getType()) == null) {
			String className =
				BioObjectUtil.stripPackageNameFromClassName(getClass().getName());
			CVTerm defaultCvTerm = c.getDefaultCVTermForClass(className);
			if (defaultCvTerm == null) {
				throw new BioObjectConfigurationException("No default set for " + className);
			}
			clone.setType(defaultCvTerm);
		}
		//clone and translate FeatureLocations
		clone.setFeatureLocations(new HashSet<FeatureLocation>());
		for (FeatureLocation loc : feature.getFeatureLocations()) {
			FeatureLocation cloneLoc = new FeatureLocation(loc);
			AbstractBioFeature srcFeat =
				(AbstractBioFeature)BioObjectUtil.createBioObject(loc.getSourceFeature(), conf);
			if (srcFeat != null) {
				cloneLoc.setSourceFeature(srcFeat.translateSimpleObjectType(c));
			}
			clone.getFeatureLocations().add(cloneLoc);
		}
		return clone;
	}
	
	private static class SimpleObjectIterator extends AbstractSimpleObjectIterator
	{		
		private AbstractBioFeature feature;
		private BioObjectConfiguration conf;
		private Iterator<AbstractBioFeatureRelationship> frIter;
		private AbstractSimpleObjectIterator soIter;
		private boolean returnedFeature;
		
		public SimpleObjectIterator(AbstractBioFeature feature, BioObjectConfiguration conf)
		{
			this.feature = feature;
			this.conf = conf;
			this.returnedFeature = false;
		}
		
		public AbstractSimpleObject peek() {
			if (!returnedFeature) {
				current = feature.translateSimpleObjectType(conf);
				return current;
			}
			if (soIter == null) {
				return null;
			}
			return current;
		}

		public boolean hasNext() {
			if (!returnedFeature) {
				return true;
			}
			if (soIter == null) {
				return false;
			}
			return soIter.hasNext();
		}

		public AbstractSimpleObject next() {
			AbstractSimpleObject retVal = null;
			if (!returnedFeature) {
				retVal = peek();
				returnedFeature = true;
				frIter = feature.getBioFeatureRelationships();
				if (frIter.hasNext()) {
					soIter = frIter.next().getWriteableSimpleObjects(conf);
				}
				return retVal;
			}
			retVal = soIter.next();
			current = retVal;
			if (!soIter.hasNext() && frIter.hasNext()) {
				soIter = frIter.next().getWriteableSimpleObjects(conf);
			}
			return retVal;
		}
	}
	
	private class BioFeatureRelationshipIterator implements Iterator<AbstractBioFeatureRelationship>
	{
		private Iterator<FeatureRelationship> frIter;
		private BioObjectConfiguration conf;
		
		public BioFeatureRelationshipIterator(AbstractBioFeature feature, BioObjectConfiguration conf)
		{
			this.conf = conf;
			frIter = feature.feature.getChildFeatureRelationships().iterator();
		}

		public AbstractBioFeatureRelationship next()
		{
			return (AbstractBioFeatureRelationship)BioObjectUtil.createBioObject(frIter.next(), conf);
		}
		
		public boolean hasNext()
		{
			return frIter.hasNext();
		}
		
		public void remove()
		{
		}
	}
}
