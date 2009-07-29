package org.obo.datamodel;

public interface Link extends Impliable, IdentifiableObject, Relationship, PathCapable {

	public LinkedObject getParent();
	
	public LinkedObject getAncestor();

	public void setParent(LinkedObject parent);

	public void setNamespace(Namespace namespace);

	public Namespace getNamespace();
}
