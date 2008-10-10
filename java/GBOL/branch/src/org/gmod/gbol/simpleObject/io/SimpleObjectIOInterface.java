package org.gmod.gbol.simpleObject.io;

import java.util.Collection;

import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureLocation;
import org.gmod.gbol.simpleObject.SimpleObjectIteratorInterface;

public interface SimpleObjectIOInterface {

	//public boolean write(Collection<AbstractSimpleObject> simpleObjects);
	public boolean write(SimpleObjectIteratorInterface iter) throws Exception;
	public Collection<? extends AbstractSimpleObject> readAll() throws Exception;
	public Collection<? extends Feature> getAllFeatures() throws Exception;
	public Collection<? extends Feature> getAllFeaturesByRange(FeatureLocation loc) throws Exception;
	public Collection<? extends Feature> getFeaturesByCVTerm(CVTerm cvterm) throws Exception;
	public Collection<? extends Feature> getFeaturesByCVTermAndRange(CVTerm cvterm, FeatureLocation loc) throws Exception;
}