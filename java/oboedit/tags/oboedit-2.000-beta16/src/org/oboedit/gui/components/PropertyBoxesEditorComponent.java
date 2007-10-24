package org.oboedit.gui.components;

import java.awt.*;
import java.util.*;
import javax.swing.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.Preferences;

public class PropertyBoxesEditorComponent extends AbstractTextEditComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7582537527616617731L;
	protected JCheckBox transitivityCheckbox = new JCheckBox("Is transitive?");
	protected JCheckBox symmetricalCheckbox = new JCheckBox("Is symmetric?");
	protected JCheckBox cyclicalCheckbox = new JCheckBox("Is cyclic?");

	public PropertyBoxesEditorComponent() {
		transitivityCheckbox.setOpaque(false);
		symmetricalCheckbox.setOpaque(false);
		cyclicalCheckbox.setOpaque(false);
	}

	@Override
	protected void loadGUI() {
		if (currentObject != null && currentObject instanceof OBOProperty) {
			OBOProperty prop = (OBOProperty) currentObject;
			transitivityCheckbox.setSelected(prop.isTransitive());
			symmetricalCheckbox.setSelected(prop.isSymmetric());
			cyclicalCheckbox.setSelected(prop.isCyclic());
		} else {
			transitivityCheckbox.setSelected(false);
			symmetricalCheckbox.setSelected(false);
			cyclicalCheckbox.setSelected(false);
		}
	}

	public void populateFields(IdentifiedObject io) {
		if (io instanceof OBOProperty) {
			OBOProperty prop = (OBOProperty) io;
			prop.setTransitive(transitivityCheckbox.isSelected());
			prop.setSymmetric(symmetricalCheckbox.isSelected());
			prop.setCyclic(cyclicalCheckbox.isSelected());
		}
	}

	public java.util.List getChanges() {
		if (currentObject != null && currentObject instanceof OBOProperty) {
			OBOProperty prop = (OBOProperty) currentObject;
			java.util.List out = new LinkedList();
			if (prop.isTransitive() != transitivityCheckbox.isSelected())
				out.add(new TransitiveHistoryItem(prop));
			if (prop.isSymmetric() != symmetricalCheckbox.isSelected())
				out.add(new SymmetricHistoryItem(prop));
			if (prop.isCyclic() != cyclicalCheckbox.isSelected())
				out.add(new CyclicHistoryItem(prop));

			return out;
		} else
			return Collections.EMPTY_LIST;
	}

	@Override
	public Component resolveName(String id, Properties props, String xml) {
		if (id.equals("transitivity_checkbox"))
			return transitivityCheckbox;
		else if (id.equals("symmetrical_checkbox"))
			return symmetricalCheckbox;
		else if (id.equals("cyclical_checkbox"))
			return cyclicalCheckbox;
		else
			return new JButton(id);
	}

	@Override
	protected void initializeGUI() {
	}

	@Override
	protected boolean useSubLayout() {
		return true;
	}

	@Override
	protected String getDefaultLayout() {
		return "<box orientation='HORZ'>" + "<glue/>"
				+ "<component id='transitivity_checkbox'/>"
				+ "<spacer orientation='horz' size='20'/>"
				+ "<component id='symmetrical_checkbox'/>"
				+ "<spacer orientation='horz' size='20'/>"
				+ "<component id='cyclical_checkbox'/>" + "<glue/>" + "</box>";
	}

	protected String getWarningLabel() {
		return "";
	}

	public String getID() {
		return "PROPERTY_BOXES_EDITOR";
	}
}
