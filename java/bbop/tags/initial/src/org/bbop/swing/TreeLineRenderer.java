package org.bbop.swing;

import javax.swing.JTree;
import java.awt.Color;

public interface TreeLineRenderer {

    public Color getLineColor(JTree tree,
			      Object value,
			      int row);
}
