package org.obo.datamodel;

import java.io.Serializable;

public interface TermSubset extends Cloneable, Serializable {

	public String getName();

	public String getDesc();

	public void setName(String name);

	public void setDesc(String desc);

	public Object clone();
}
