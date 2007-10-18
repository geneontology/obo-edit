package org.oboedit.graph;

import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;

import org.oboedit.gui.DropUtil;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Selection;
import org.oboedit.gui.components.LinkDatabaseCanvas;

public class VisibilityDropBehavior implements ViewBehavior {
	protected DropTargetListener dropListener = new DropTargetListener() {

		public void dragEnter(DropTargetDragEvent dtde) {
		}

		public void dragExit(DropTargetEvent dte) {
		}

		public void dragOver(DropTargetDragEvent dtde) {
		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}

		public void drop(DropTargetDropEvent dtde) {
			try {
				GestureTarget target = canvas.getTarget(dtde.getLocation().x,
						dtde.getLocation().y);
				if (target.isEmpty()) {
					dtde.acceptDrop(DnDConstants.ACTION_COPY);
					Selection s = DropUtil.getSelection(dtde);
					canvas.addVisibleObjects(s.getAllSelectedObjects());
					dtde.dropComplete(true);
				}
			} catch (InvalidDnDOperationException ex) {
				// ignore it
			}
		}
	};

	protected LinkDatabaseCanvas canvas;

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addDropTargetListener(dropListener);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeDropTargetListener(dropListener);
	}

}
