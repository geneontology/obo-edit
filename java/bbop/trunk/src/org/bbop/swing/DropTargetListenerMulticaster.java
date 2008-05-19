package org.bbop.swing;

import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.util.Collection;
import java.util.LinkedList;

/**
 * An implementation of {@link java.awt.dnd.DropTargetListener} that multicasts
 * drag and drop events to sub-listeners
 * 
 * @author jrichter
 *
 */
import org.apache.log4j.*;

public class DropTargetListenerMulticaster implements DropTargetListener {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DropTargetListenerMulticaster.class);

	protected Collection<DropTargetListener> listeners = new LinkedList<DropTargetListener>();

	public void addListener(DropTargetListener listener) {
		listeners.add(listener);
	}

	public void removeListener(DropTargetListener listener) {
		listeners.remove(listener);
	}

	public void dragEnter(DropTargetDragEvent dtde) {
		for (DropTargetListener listener : listeners) {
			listener.dragEnter(dtde);
		}
	}

	public void dragExit(DropTargetEvent dte) {
		for (DropTargetListener listener : listeners) {
			listener.dragExit(dte);
		}
	}

	public void dragOver(DropTargetDragEvent dtde) {
		for (DropTargetListener listener : listeners) {
			listener.dragOver(dtde);
		}
	}

	public void drop(DropTargetDropEvent dtde) {
		for (DropTargetListener listener : listeners) {
			listener.drop(dtde);
		}
	}

	public void dropActionChanged(DropTargetDragEvent dtde) {
		for (DropTargetListener listener : listeners) {
			listener.dropActionChanged(dtde);
		}
	}

}
