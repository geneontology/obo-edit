package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;
import javax.swing.*;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class NamespaceEditorComponent extends AbstractTextEditComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NamespaceEditorComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 8387517031428288469L;
	protected JComboBox namespaceList = new JComboBox();

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("list"))
			return namespaceList;
		else
			return new JButton("id");
	}

	public NamespaceEditorComponent() {
	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<box orientation='HORZ'><label text='Namespace'/>"
				+ "<spacer orientation='horz' size='10'/>"
				+ "<component id='list'/></box>";
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null) {
			boolean enable = !TermUtil.isObsolete(currentObject);

			namespaceList.setEnabled(true);
			namespaceList.removeAllItems();
			ArrayList namespaces = new ArrayList();
			namespaces.addAll(SessionManager.getManager().getSession()
					.getNamespaces());
			Collections.sort(namespaces, Namespace.COMPARATOR);
			Iterator it = namespaces.iterator();
			while (it.hasNext()) {
				Namespace ns = (Namespace) it.next();
				namespaceList.addItem(ns);
			}
			namespaceList.setSelectedItem(currentObject.getNamespace());
			namespaceList.setEnabled(enable);
		} else {
			namespaceList.setEnabled(false);
			namespaceList.setSelectedItem(null);
		}
	}

	@Override
	protected void initializeGUI() {
		namespaceList.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				(int) namespaceList.getPreferredSize().getHeight()));
		setMaximumSize(new Dimension(Integer.MAX_VALUE,
				(int) getPreferredSize().getHeight()));
		setMinimumSize(getPreferredSize());
	}

	public String getID() {
		return "NAMESPACE_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
		io.setNamespace((Namespace) namespaceList.getSelectedItem());
	}

	public java.util.List getChanges() {
		if (currentObject != null) {
			if (!ObjectUtil.equals(namespaceList.getSelectedItem(),
					currentObject.getNamespace())) {
				HistoryItem item = new NamespaceHistoryItem(currentObject,
						(Namespace) namespaceList.getSelectedItem());
				return Collections.singletonList(item);
			} else {
				return Collections.EMPTY_LIST;
			}
		} else
			return Collections.EMPTY_LIST;
	}
}
