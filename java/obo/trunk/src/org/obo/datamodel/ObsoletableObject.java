package org.obo.datamodel;

import java.util.Set;

public interface ObsoletableObject extends IdentifiedObject {

	public boolean isObsolete();

	public void setObsolete(boolean isObsolete);

	public Set<ObsoletableObject> getReplacedBy();

	public void addReplacedBy(ObsoletableObject o);

	public void removeReplacedBy(ObsoletableObject o);

	public Set<ObsoletableObject> getConsiderReplacements();

	public void addConsiderReplacement(ObsoletableObject o);

	public void removeConsiderReplacement(ObsoletableObject o);

	public NestedValue getConsiderExtension(ObsoletableObject o);

	public void addConsiderExtension(ObsoletableObject o, NestedValue v);

	public NestedValue getReplacedByExtension(ObsoletableObject o);

	public void addReplacedByExtension(ObsoletableObject o, NestedValue v);

	public void setObsoleteExtension(NestedValue nv);

	public NestedValue getObsoleteExtension();

}
