package org.obo.datamodel;

public interface NamespacedObject {
	public void setNamespace(Namespace namespace);

	public Namespace getNamespace();

	public NestedValue getNamespaceExtension();

	public void setNamespaceExtension(NestedValue nv);
}
