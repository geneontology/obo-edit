package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;
import javax.swing.*;

import org.obo.datamodel.*;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.Preferences;

public class IDEditorComponent extends AbstractTextEditComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected JTextField idLabel = new JTextField();
	protected JTextField secondaryIDLabel = new JTextField();

	protected Component secondaryIDComponent;
	protected Container parentContainer;
	protected int secondaryComponentIndex = -1;
	protected boolean secondariesVisible = true;

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		System.err.println("calling resolveName " + id);
		if (id.equals("id_label"))
			return idLabel;
		else if (id.equals("secondary_id_label"))
			return secondaryIDLabel;
		else
			return new JButton("id");
	}

	public IDEditorComponent() {
		idLabel.setEditable(false);
		idLabel.setBorder(null);
		idLabel.setOpaque(false);
		secondaryIDLabel.setEditable(false);
		secondaryIDLabel.setBorder(null);
		secondaryIDLabel.setOpaque(false);
	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<box orientation='vert'>"
				+ "<box orientation='HORZ'>"
				+ "<label text='PEID'/><spacer orientation='horz' size='10'/>"
				+ "<component id='id_label'/></box>"
				+ "<box orientation='HORZ' name='secondary_ids'>"
				+ "<label text='Secondary IDs'/><spacer orientation='horz' size='10'/>"
				+ "<component id='secondary_id_label'/></box>" + "</box>";
	}

	@Override
	public boolean isSingleton() {
		return false;
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null) {
			idLabel.setEnabled(true);
			idLabel.setText(currentObject.getID());
			secondaryIDLabel.setEnabled(true);
			if (currentObject instanceof MultiIDObject) {
				java.util.List list = new ArrayList();
				list.addAll(((MultiIDObject) currentObject).getSecondaryIDs());
				setSecondariesVisible(list.size() > 0);
				Collections.sort(list);
				StringBuffer sb = new StringBuffer();
				Iterator it = list.iterator();
				for (int i = 0; it.hasNext(); i++) {
					String s = (String) it.next();
					if (i > 0)
						sb.append(", ");
					sb.append(s);
				}
				secondaryIDLabel.setText(sb.toString());
				secondaryIDLabel.repaint();
			}
		} else {
			idLabel.setEnabled(false);
			idLabel.setText("<no selection>");
			secondaryIDLabel.setEnabled(false);
			secondaryIDLabel.setText("<no selection>");
		}
	}

	protected void setSecondariesVisible(boolean visible) {
		if (secondariesVisible != visible && secondaryIDComponent != null
				&& parentContainer != null && secondaryComponentIndex >= 0) {

			if (visible) {
				parentContainer.add(secondaryIDComponent,
						secondaryComponentIndex);
			} else {
				parentContainer.remove(secondaryIDComponent);
			}
			validateTree();
			repaint();
			this.secondariesVisible = visible;
		}
	}

	@Override
	protected void initializeGUI() {
		idLabel.setMaximumSize(new Dimension(Integer.MAX_VALUE, (int) idLabel
				.getPreferredSize().getHeight()));

		secondaryIDLabel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				(int) secondaryIDLabel.getPreferredSize().getHeight()));
		setMaximumSize(new Dimension(Integer.MAX_VALUE,
				(int) getPreferredSize().getHeight()));
	}

	public String getID() {
		return "ID_EDITOR";
	}

	public void populateFields(IdentifiedObject io) {
	}

	public java.util.List getChanges() {
		return Collections.EMPTY_LIST;
	}
}
