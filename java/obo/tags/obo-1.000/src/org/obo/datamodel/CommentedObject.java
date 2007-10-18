package org.obo.datamodel;

public interface CommentedObject extends IdentifiedObject {

	public String getComment();

	public void setComment(String comment);

	public NestedValue getCommentExtension();

	public void setCommentExtension(NestedValue nv);

}
