package org.obo.datamodel;

import java.io.Serializable;

public interface AnnotatedObject extends IdentifiedObject, ModificationMetadataObject, MultiIDObject,
		SynonymedObject, DbxrefedObject, CommentedObject, ObsoletableObject,
		Cloneable, Serializable, DefinedObject {

}
