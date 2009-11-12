package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

public class ParentRelationEntry<R>
{
	private String parentTermId;
	private R relationShipType;

	public ParentRelationEntry(String parentTerm, R relationShipType)
	{
		super();
		this.parentTermId = parentTerm;
		this.relationShipType = relationShipType;
	}



	public String getParentTermId()
	{
		return parentTermId;
	}

	public void setParentTermId(String parentTermId)
	{
		this.parentTermId = parentTermId;
	}

	public R getRelationShipType()
	{
		return relationShipType;
	}

	public void setRelationShipType(R relationShipType)
	{
		this.relationShipType = relationShipType;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + ((parentTermId == null) ? 0 : parentTermId.hashCode());
		result = prime * result + ((relationShipType == null) ? 0 : relationShipType.hashCode());
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ParentRelationEntry<R> other = (ParentRelationEntry<R>) obj;
		if (parentTermId == null) {
			if (other.parentTermId != null)
				return false;
		}
		else if (!parentTermId.equals(other.parentTermId))
			return false;
		if (relationShipType == null) {
			if (other.relationShipType != null)
				return false;
		}
		else if (!relationShipType.equals(other.relationShipType))
			return false;
		return true;
	}

}
