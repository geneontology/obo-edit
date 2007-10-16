package org.oboedit.gui;

import java.awt.Point;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.io.IOException;

public class DropUtil {
	public static Selection getSelection(Transferable t) {
		Object transferData = null;
		try {
			transferData = t.getTransferData(SelectionTransferHandler.SELECTION_FLAVOR);
		} catch (UnsupportedFlavorException e1) {
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		if (transferData instanceof Selection)
			return (Selection) transferData;
		else
			return null;			
	}
	
	public static Point getLocation(DropTargetEvent e) {
		if (e instanceof DropTargetDragEvent)
			return getLocation((DropTargetDragEvent) e);
		if (e instanceof DropTargetDropEvent)
			return getLocation((DropTargetDropEvent) e);
		return null;				
	}
	
	public static Point getLocation(DropTargetDragEvent e) {
		return e.getLocation();
	
	}
	public static Point getLocation(DropTargetDropEvent e) {
		return e.getLocation();
	}
	
	public static Selection getSelection(DropTargetEvent e) {
		if (e instanceof DropTargetDragEvent)
			return getSelection((DropTargetDragEvent) e);
		if (e instanceof DropTargetDropEvent)
			return getSelection((DropTargetDropEvent) e);
		return null;		
	}
	
	public static Selection getSelection(DropTargetDragEvent e) {
		Transferable t = e.getTransferable();
		return getSelection(t);
	}
	
	public static Selection getSelection(DropTargetDropEvent e) {
		Transferable t = e.getTransferable();
		return getSelection(t);
	}
}
