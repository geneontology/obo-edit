package org.oboedit.gui.filter;

public abstract class AbstractRendererSpecField<T> implements
		GeneralRendererSpecField<T> {

	public int hashCode() {
		return getID().hashCode();
	}
	
	public boolean equals(Object o) {
		if (!(o instanceof GeneralRendererSpecField<?>))
			return false;
		else
			return ((GeneralRendererSpecField<?>) o).getID().equals(getID());
	}

}
