package org.oboedit.gui.components.treeView;

import javax.swing.JTree;
import javax.swing.tree.TreePath;

/**
 * 
 * @author John Day-Richter, Jennifer Deegan, and Nicolas Rodriguez.<br>
 * Docs by Jennifer Deegan and Nicolas Rodriguez.<br>
 * 
 * VisibleRunnable class<br>
 * 
 * The constructor takes a TreePath argument called path that defines the path of the current object and assigns it to
 * a protected TreePath variable 'path'.<br>
 * 
 * Contains code for threading and for expansion of the tree. 
 * 
 */

public class VisibleRunnable extends JTree implements Runnable {
	protected TreePath path;

	public VisibleRunnable(TreePath path) {

		this.path = path;
		//logger.debug("VisibleRunnable class: constructor.");
	}

	/**
	 */
	public void run() {
		makeVisible(path);
		//logger.debug("VisibleRunnable class: run method.");

	}

	boolean expandAllowed = true;

	/**
	 * makeVisible method is duplicated from RestrictedJTree and maybe should  be 
	 * taken out into an abstract class so they can share it. 
	 * It overrides the version in JTree, though I cannot access the code of JTree to see what is changed.
	 *
	 * It's not clear to me yet what super.makeVisible(path) is for.
	 *
	 */

	@Override
	public void makeVisible(TreePath path) {
		expandAllowed = true;
		super.makeVisible(path);  
		expandAllowed = false;
		//logger.debug("VisibleRunnable: makeVisible method.");	

	}



}