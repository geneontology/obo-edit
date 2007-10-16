package org.bbop.swing.tablelist;

import java.awt.event.ActionListener;

public interface ListTableEditor<T> {

	public void notifyCancel();
	public void notifyActive();
	public T getValue();
	public void setValue(T value);
	public T createNewValue();
	public void addCommitListener(ActionListener listener);
	public void removeCommitListener(ActionListener listener);
}
