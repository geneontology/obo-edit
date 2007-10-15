package org.bbop.swing;

import java.awt.dnd.DragSourceDragEvent;
import javax.swing.Icon;

public interface DragImageGenerator {
	public Icon getImage(DragSourceDragEvent event);
}
