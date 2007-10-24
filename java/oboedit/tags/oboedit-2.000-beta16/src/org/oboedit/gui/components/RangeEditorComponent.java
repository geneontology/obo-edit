package org.oboedit.gui.components;

import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.IOException;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.tree.*;

import org.bbop.swing.*;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.DropUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;

public class RangeEditorComponent extends AbstractTextEditComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Border lineBorder = new LineBorder(Color.black);
	protected Border oldBorder;
	protected JButton rangeButton = new JButton("<no range>");
	protected Type range;

	/*
	protected DropListener dropRangeListener = new DropAdapter() {
		@Override
		public boolean allowDrop(DragEvent e) {
			TreePath[] paths = (TreePath[]) e.getData();
			if (paths.length == 1) {
				LinkedObject termRange = ((Link) paths[0]
						.getLastPathComponent()).getChild();
				return !currentObject.equals(termRange);
			} else
				return false;
		}

		@Override
		public void dragEnter(DragEvent e) {
			oldBorder = rangeButton.getBorder();
			rangeButton.setBorder(lineBorder);
		}

		@Override
		public void dragExit(DragEvent e) {
			rangeButton.setBorder(oldBorder);
		}

		@Override
		public void drop(DragEvent e) {
			TreePath[] paths = (TreePath[]) e.getData();
			if (paths.length == 1
					&& ((Link) paths[0].getLastPathComponent()).getChild() instanceof Type) {
				setRange((Type) ((Link) paths[0].getLastPathComponent())
						.getChild());
			}
			rangeButton.setBorder(oldBorder);
		}
	};

	protected DropTarget dropTarget = new DropTarget(rangeButton,
			dropRangeListener);
			*/
	protected DropTargetListener dropRangeListener = new DropTargetListener() {

		public void drop(DropTargetDropEvent dtde) {
			Selection selection = DropUtil.getSelection(dtde);
			Type range = (Type) selection.getTermSubSelection();
			setRange(range);
			rangeButton.setBorder(oldBorder);
		}
		
		public void dragOver(DropTargetDragEvent dtde) {
			Selection s = DropUtil.getSelection(dtde);
			if (!(s.getTermSubSelection() instanceof Type))
				dtde.rejectDrag();			
		}

		public void dragEnter(DropTargetDragEvent dtde) {
			Selection s = DropUtil.getSelection(dtde);
			if (!(s.getTermSubSelection() instanceof Type))
				dtde.rejectDrag();
			oldBorder = rangeButton.getBorder();
			rangeButton.setBorder(lineBorder);
		}

		public void dragExit(DropTargetEvent dte) {
			rangeButton.setBorder(oldBorder);
		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}		
	};

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("range_button"))
			return rangeButton;
		else
			return new JButton(id);
	}

	public RangeEditorComponent() {

	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<box orientation='HORZ'><label text='Range'/><spacer orientation='horz' size='10'/><component id='range_button'/></box>";
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null && currentObject instanceof OBOProperty) {
			setRange(((OBOProperty) currentObject).getRange());
		} else {
			setRange(null);
		}
	}

	public void setRange(Type range) {
		this.range = range;
		if (range == null)
			rangeButton.setLabel("<no range specified>");
		else
			rangeButton.setLabel(range.getName() + " (" + range.getID() + ")");
		rangeButton.setMinimumSize(rangeButton.getPreferredSize());
	}

	protected String getWarningLabel() {
		return "";
	}
	
	@Override
	protected void installListeners() {
		setDropTarget(new DropTarget(this, dropRangeListener));
	}

	@Override
	protected void uninstallListeners() {
		setDropTarget(null);
	}

	@Override
	protected void initializeGUI() {
		setMinimumSize(getPreferredSize());
	}

	public String getID() {
		return "RANGE_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof OBOProperty) {
			((OBOProperty) io).setRange(range);
		}
	}

	public java.util.List getChanges() {
		if (currentObject != null && currentObject instanceof OBOProperty) {
			java.util.List out = new LinkedList();
			if (!ObjectUtil.equals(range, ((OBOProperty) currentObject)
					.getRange())) {
				out
						.add(new RangeHistoryItem((OBOProperty) currentObject,
								range));
			}
			return out;
		} else
			return Collections.EMPTY_LIST;
	}
}
