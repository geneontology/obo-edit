package org.bbop.swing.dragtree.event;

import java.util.EventListener;

public interface DragTreeEventListener extends EventListener {

    public void rightClick(DragTreeEvent e);

    public void drag(DragTreeEvent e);

    public void drop(DragTreeEvent e);
}
