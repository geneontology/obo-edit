package org.obo.postcomp;

import org.obo.datamodel.OBOProperty;

/**
 * This class is used exclusively by {@link PostcompUtil} to map {@link OBOProperty}
 * names to names that fit grammatically into an automatically generated
 * post-composition name
 */
public interface TypeNameProvider {
	
	/**
	 * Returns a name for the given OBOProperty
	 */
	public String getName(OBOProperty p);
}
