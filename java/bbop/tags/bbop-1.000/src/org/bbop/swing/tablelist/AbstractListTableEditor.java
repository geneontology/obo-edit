package org.bbop.swing.tablelist;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JPanel;

public abstract class AbstractListTableEditor<T> extends JPanel implements
		ListTableEditor<T> {

	protected List<ActionListener> commitListeners = new LinkedList<ActionListener>();

	public void commit() {
		fireCommit();
	}

	public void addCommitListener(ActionListener listener) {
		commitListeners.add(listener);
	}

	public void removeCommitListener(ActionListener listener) {
		commitListeners.remove(listener);
	}

	protected void fireCommit() {
		ActionEvent e = new ActionEvent(this, 0, "commit");
		for (ActionListener l : commitListeners) {
			l.actionPerformed(e);
		}
	}

	public void notifyActive() {
	}

	public void notifyCancel() {
	}
}
