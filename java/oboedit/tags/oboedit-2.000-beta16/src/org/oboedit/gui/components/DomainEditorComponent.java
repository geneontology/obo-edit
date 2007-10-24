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
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.DropUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;

public class DomainEditorComponent extends AbstractTextEditComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Border lineBorder = new LineBorder(Color.black);
	protected Border oldBorder;
	protected JButton domainButton = new JButton("<no domain>");
	protected IdentifiedObject domain;

	protected DropTargetListener dropDomainListener = new DropTargetListener() {

		public void drop(DropTargetDropEvent dtde) {
			Selection selection = DropUtil.getSelection(dtde);
			Type domain = (Type) selection.getTermSubSelection();
			setDomain(domain);
			domainButton.setBorder(oldBorder);
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
			oldBorder = domainButton.getBorder();
			domainButton.setBorder(lineBorder);
		}

		public void dragExit(DropTargetEvent dte) {
			domainButton.setBorder(oldBorder);
		}

		public void dropActionChanged(DropTargetDragEvent dtde) {
		}		
	};


	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("domain_button"))
			return domainButton;
		else
			return new JButton(id);
	}

	public DomainEditorComponent() {

	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<box orientation='HORZ'><label text='Domain'/><spacer orientation='horz' size='10'/><component id='domain_button'/></box>";
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null && currentObject instanceof OBOProperty) {
			setDomain(((OBOProperty) currentObject).getDomain());
		} else {
			setDomain(null);
		}
	}

	public void setDomain(IdentifiedObject domain) {
		this.domain = domain;
		if (domain == null)
			domainButton.setLabel("<no domain specified>");
		else
			domainButton.setLabel(domain.getName() + " (" + domain.getID()
					+ ")");
		domainButton.setMinimumSize(domainButton.getPreferredSize());
	}

	protected String getWarningLabel() {
		return "";
	}

	@Override
	protected void installListeners() {
		setDropTarget(new DropTarget(this, dropDomainListener));
	}

	@Override
	protected void uninstallListeners() {
		setDropTarget(null);
	}

	@Override
	protected void initializeGUI() {
		setMinimumSize(getPreferredSize());
	}

	public java.util.List getWarnings() {
		return Collections.EMPTY_LIST;
	}

	public String getID() {
		return "DOMAIN_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof OBOProperty) {
			((OBOProperty) io).setDomain(domain);
		}
	}

	public java.util.List getChanges() {
		if (currentObject != null && currentObject instanceof OBOProperty) {
			java.util.List out = new LinkedList();
			if (!ObjectUtil.equals(domain, ((OBOProperty) currentObject)
					.getDomain())) {
				out.add(new DomainHistoryItem((OBOProperty) currentObject,
						domain));
			}
			return out;
		} else
			return Collections.EMPTY_LIST;
	}
}
