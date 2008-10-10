package org.gmod.gbol.bioObject.util;

import java.lang.reflect.InvocationTargetException;

import org.gmod.gbol.bioObject.AbstractBioObject;
import org.gmod.gbol.bioObject.conf.BioObjectConfiguration;
import org.gmod.gbol.bioObject.conf.BioObjectConfigurationException;
import org.gmod.gbol.simpleObject.AbstractSimpleObject;
import org.gmod.gbol.simpleObject.CVTerm;
import org.gmod.gbol.simpleObject.Feature;
import org.gmod.gbol.simpleObject.FeatureRelationship;

/** Class providing utility methods for Bio objects.
 * 
 * @author elee
 *
 */
public class BioObjectUtil {

	private BioObjectUtil()
	{
	}
	
	/** Remove the package identifier from the fully qualified class name
	 *  (e.g. my.package.className returns className).  If a class name is passed
	 *  with no package identifier, it just returns the class name.
	 * 
	 * @param className - Class name with fully qualified package identifier
	 * @return Class name without fully qualified package identifier
	 */
	public static String stripPackageNameFromClassName(String className)
	{
		return className.substring(className.lastIndexOf('.') + 1);
	}
	
	/** Generic method for creating AbstractBioObjects from AbstractSimpleObjects.  Makes use of 
	 *  reflection and the passed configuration to figure out how to instantiate the AbstractBioObject.
	 * 
	 * @param simpleObject - AbstractSimpleObject to create the AbstractBioObject from
	 * @param conf - BioObjectConfiguration containing information on how to map Simple->Bio objects
	 * @return AbstractBioObject corresponding to the AbstractSimpleObject
	 */
	public static AbstractBioObject createBioObject(AbstractSimpleObject simpleObject,
			BioObjectConfiguration conf)
	{
		if (!(simpleObject instanceof Feature) && !(simpleObject instanceof FeatureRelationship)) {
			return null;
		}
		CVTerm cvterm;
		if (simpleObject instanceof Feature) {
			cvterm = ((Feature)simpleObject).getType();
		}
		else {
			cvterm = ((FeatureRelationship)simpleObject).getType();
		}
		String className = conf.getClassForCVTerm(cvterm);
		if (className == null) {
			throw new BioObjectConfigurationException(cvterm.getName() +
					" does not exist in configuration");
		}
		String pkg = AbstractBioObject.class.getPackage().getName();
		try {
			Class<?> clazz = Class.forName(pkg + "." + className);
			return (AbstractBioObject)clazz.getConstructor(simpleObject instanceof Feature ?
					Feature.class : FeatureRelationship.class,
					BioObjectConfiguration.class).newInstance(simpleObject, conf);
		}
		catch (ClassNotFoundException e) {
			throw new BioObjectConfigurationException(className + " is not a valid GBOL class type");
		}
		catch (NoSuchMethodException e) {
			throw new RuntimeException("Constructor for GBOL object not found: " + e.getMessage());
		}
		catch (InstantiationException e) {
			throw new RuntimeException("Instantiating GBOL object failed: " + e.getMessage());
		}
		catch (IllegalArgumentException e) {
			throw new RuntimeException("Instantiating GBOL object failed: " + e.getMessage());
		}
		catch (InvocationTargetException e) {
			throw new RuntimeException("Instantiating GBOL object failed: " + e.getMessage());
		}
		catch (IllegalAccessException e) {
			throw new RuntimeException("Instantiating GBOL object failed: " + e.getMessage());
		}
	}

}
