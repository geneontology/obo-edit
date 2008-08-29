package org.oboedit.gui.components.treeView;

import javax.swing.tree.TreePath;

public class VisibleRunnable implements Runnable {
	protected TreePath path;

	public VisibleRunnable(TreePath path) {
		this.path = path;
	System.out.println("VisibleRunnable class: constructor.");
	}

	public void run() {
		//makeVisible(path);
		System.out.println("VisibleRunnable class: run method.");

	}
}