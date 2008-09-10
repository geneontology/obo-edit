package org.gmod.gbol.simpleObject;

import java.util.ArrayList;
import java.util.List;

/*
 * Autogenerated extension of org.gmod.gbol.simpleObject.generated.Feature.
 * Add custom code to this file. 
 *
 * Author: Robert Bruggner, rbruggner@berkeleybop.org
 *
*/

public class Feature extends org.gmod.gbol.simpleObject.generated.AbstractFeature {

	private static final long serialVersionUID = 1L;
	
	public boolean addFeatureProperty(CVTerm type,String value){
		FeatureProperty featureProperty = new FeatureProperty();
		featureProperty.setType(type);
		featureProperty.setValue(value);
		featureProperty.setFeature(this);
		return this.getFeatureProperties().add(featureProperty);
	}
	
	public boolean addFeatureLocation(FeatureLocation featureLocation){
		featureLocation.setFeature(this);
		return this.getFeatureLocations().add(featureLocation);
	}
	
	public boolean addSynonym(CVTerm type, String id){
		FeatureSynonym fs = new FeatureSynonym();
		fs.setFeature(this);
		Synonym s = new Synonym();
		s.setType(type);
		s.setName(id);
		fs.setSynonym(s);
		return this.getFeatureSynonyms().add(fs);
	}
	
	public boolean addDBXref(DBXref dbxref){
		FeatureDBXref fdbx = new FeatureDBXref();
		fdbx.setFeature(this);
		fdbx.setDbxref(dbxref);
		fdbx.setIsCurrent(true);
		return this.getFeatureDBXrefs().add(fdbx);
	}

	public List<FeatureProperty> getFeaturePropertiesByType(CVTerm type){
		List<FeatureProperty> featureProperties = new ArrayList<FeatureProperty>();
		for (FeatureProperty fp : this.getFeatureProperties()){
			if (fp.getType().equals(type)){
				featureProperties.add(fp);
			}
		}
		return featureProperties;
	}
}
