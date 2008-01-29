package org.obo.datamodel;

import java.io.*;

/** This is a relationship between terms/classes - not instances */
public interface OBORestriction extends Link, Cloneable,
		Serializable {

	public void setCardinality(Integer cardinality);

	public void setMaxCardinality(Integer maxCardinality);

	public void setMinCardinality(Integer minCardinality);

	public Integer getCardinality();

	public Integer getMaxCardinality();

	public Integer getMinCardinality();

	public boolean completes();

	public boolean inverseCompletes();

	public void setInverseCompletes(boolean inverseCompletes);

	public void setCompletes(boolean completes);

	public void setNecessarilyTrue(boolean necessarilyTrue);

	public void setInverseNecessarilyTrue(boolean inverseNecessarilyTrue);

	public boolean isNecessarilyTrue();

	public boolean isInverseNecessarilyTrue();
}
