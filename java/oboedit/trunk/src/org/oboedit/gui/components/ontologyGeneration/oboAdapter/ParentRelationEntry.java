package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

public class ParentRelationEntry<T, R>
{
	private T parentTerm;
	private R relationShipType;

	public ParentRelationEntry(T parentTerm, R relationShipType)
	{
		super();
		this.parentTerm = parentTerm;
		this.relationShipType = relationShipType;
	}



	public T getParentTerm()
	{
		return parentTerm;
	}

	public void setParentTerm(T parentTerm)
	{
		this.parentTerm = parentTerm;
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
		result = prime * result + ((parentTerm == null) ? 0 : parentTerm.hashCode());
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
		ParentRelationEntry<T,R> other = (ParentRelationEntry<T,R>) obj;
		if (parentTerm == null) {
			if (other.parentTerm != null)
				return false;
		}
		else if (!parentTerm.equals(other.parentTerm))
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
