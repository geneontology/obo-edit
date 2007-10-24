package org.oboedit.gui;

import java.awt.*;
import javax.swing.tree.*;

public interface LineRenderer {
	public void paintLine(Graphics g, Component c, int y, int left, int right,
			boolean isLeaf, TreePath path);
}
