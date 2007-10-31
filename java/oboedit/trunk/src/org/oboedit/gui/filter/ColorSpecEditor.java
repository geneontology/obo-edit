package org.oboedit.gui.filter;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JPanel;

public class ColorSpecEditor extends JPanel implements
		GeneralRendererSpecFieldEditor<ConfiguredColor> {

	protected JCheckBox blendBox = new JCheckBox("Do blending");
	protected JButton colorButton = new JButton(" ");

	public ColorSpecEditor() {
		colorButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Color color = JColorChooser.showDialog(ColorSpecEditor.this,
						"Choose a color", colorButton.getBackground());
				colorButton.setBackground(color);
			}
		});
		colorButton.setBorderPainted(false);
		colorButton.setBackground(Color.red);
		setLayout(new BorderLayout());
		add(blendBox, "South");
		add(colorButton, "Center");
	}

	public void setValue(ConfiguredColor o) {
		if (o != null) {
			colorButton.setBackground(o.getColor());
			blendBox.setSelected(o.isDoBlend());
		} else {
			colorButton.setBackground(Color.red);
			blendBox.setSelected(false);
		}
	}

	public ConfiguredColor getValue() {
		return new ConfiguredColor(colorButton.getBackground(), blendBox
				.isSelected());
	}

}
