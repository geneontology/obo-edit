package org.oboedit.gui.filter;

import java.awt.Component;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridLayout;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JPanel;

public class FontFaceEditor extends JPanel implements
		GeneralRendererSpecFieldEditor<String> {
	
	protected static class FontListCellRenderer extends DefaultListCellRenderer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		@Override
		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			super.getListCellRendererComponent(list, value, index, isSelected,
					cellHasFocus);
			Font newFont = new Font((String) value, 0, getFont().getSize());
			setFont(newFont);
			return this;
		}
	}

	protected JComboBox fontList = new JComboBox(GraphicsEnvironment
			.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
	
	public FontFaceEditor() {
		setLayout(new GridLayout(1,1));
		fontList.setRenderer(new FontListCellRenderer());
		add(fontList);
	}

	public String getValue() {
		return fontList.getSelectedItem().toString();
	}

	public void setValue(String o) {
		if (o != null)
			fontList.setSelectedItem(o);
		else
			fontList.setSelectedItem("Dialog");
	}

}
