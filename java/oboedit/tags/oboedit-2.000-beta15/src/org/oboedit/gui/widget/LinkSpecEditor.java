package org.oboedit.gui.widget;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import org.obo.filters.*;

public class LinkSpecEditor extends JPanel implements SpecEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected LinkRenderSpec spec = new LinkRenderSpec();

	protected JCheckBox colorBox = new JCheckBox();
	protected JCheckBox linkTypeBox = new JCheckBox();
	protected JCheckBox lineWidthBox = new JCheckBox();

	protected JLabel colorLabel = new JLabel("Change color");
	protected JLabel linkTypeLabel = new JLabel("Change line type");
	protected JLabel lineWidthLabel = new JLabel("Change line width");

	protected JPanel colorPanel = new JPanel();
	protected JPanel lineTypePanel = new JPanel();
	protected JPanel lineWidthPanel = new JPanel();

	protected JComboBox lineTypeSelector = new JComboBox();
	protected JTextField lineWidthField = new JTextField(3);
	protected JButton colorButton = new JButton();

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (colorLabel != null)
			colorLabel.setFont(font);
		if (linkTypeLabel != null)
			linkTypeLabel.setFont(font);
		if (lineWidthLabel != null)
			lineWidthLabel.setFont(font);
		if (lineTypeSelector != null)
			lineTypeSelector.setFont(font);
	}

	public RenderSpec createNewSpec() {
		return new ObjectRenderSpec();
	}

	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		linkTypeBox.setEnabled(enabled);
		lineWidthBox.setEnabled(enabled);
		colorBox.setEnabled(enabled);
		colorLabel.setEnabled(enabled);
		linkTypeLabel.setEnabled(enabled);
		lineWidthLabel.setEnabled(enabled);
		lineTypeSelector.setEnabled(enabled);
		lineWidthField.setEnabled(enabled);
		colorButton.setEnabled(enabled);
	}

	protected void updatePanels() {
		if (colorBox.isSelected())
			colorPanel.add(colorButton);
		else
			colorPanel.remove(colorButton);

		if (linkTypeBox.isSelected())
			lineTypePanel.add(lineTypeSelector);
		else
			lineTypePanel.remove(lineTypeSelector);

		if (lineWidthBox.isSelected())
			lineWidthPanel.add(lineWidthField);
		else
			lineWidthPanel.remove(lineWidthField);
		revalidate();
		repaint();

	}

	public void setButtonColor(Color buttonColor) {
		lineTypeSelector.setBackground(buttonColor);
	}

	public LinkSpecEditor() {
		setOpaque(false);
		colorButton.setBorderPainted(false);

		colorBox.setOpaque(false);
		linkTypeBox.setOpaque(false);
		lineWidthBox.setOpaque(false);

		lineTypeSelector.addItem("Solid");
		lineTypeSelector.addItem("Wavy");
		lineTypeSelector.addItem("Dashed");

		colorBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updatePanels();
			}
		});

		linkTypeBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updatePanels();
			}
		});

		lineWidthBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updatePanels();
			}
		});

		colorButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Color color = JColorChooser.showDialog(LinkSpecEditor.this,
						"Choose a color", colorButton.getBackground());
				colorButton.setBackground(color);
			}
		});

		colorPanel.setOpaque(false);
		lineTypePanel.setOpaque(false);
		lineWidthPanel.setOpaque(false);

		colorPanel.setLayout(new BoxLayout(colorPanel, BoxLayout.X_AXIS));
		lineTypePanel.setLayout(new BoxLayout(lineTypePanel, BoxLayout.X_AXIS));
		lineWidthPanel
				.setLayout(new BoxLayout(lineWidthPanel, BoxLayout.X_AXIS));

		colorPanel.add(colorLabel);
		colorPanel.add(Box.createHorizontalStrut(10));
		colorPanel.add(colorBox);
		colorPanel.add(Box.createHorizontalStrut(10));
		colorPanel.add(Box.createHorizontalGlue());

		lineTypePanel.add(linkTypeLabel);
		lineTypePanel.add(Box.createHorizontalStrut(10));
		lineTypePanel.add(linkTypeBox);
		lineTypePanel.add(Box.createHorizontalStrut(10));
		lineTypePanel.add(Box.createHorizontalGlue());

		lineWidthPanel.add(lineWidthLabel);
		lineWidthPanel.add(Box.createHorizontalStrut(10));
		lineWidthPanel.add(lineWidthBox);
		lineWidthPanel.add(Box.createHorizontalStrut(10));
		lineWidthPanel.add(Box.createHorizontalGlue());

		colorPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		lineTypePanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		lineWidthPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(colorPanel);
		add(lineTypePanel);
		add(lineWidthPanel);
		add(Box.createVerticalGlue());
	}

	protected int getLineHeight() {
		return getFont().getSize() * 2;
	}

	public RenderSpec getSpec() {
		acceptEdits();
		return spec;
	}

	protected void acceptEdits() {
		if (colorBox.isSelected())
			spec.setLinkColor(colorButton.getBackground());
		else
			spec.setLinkColor(null);

		if (linkTypeBox.isSelected())
			spec.setLineType(lineTypeSelector.getSelectedIndex());
		else
			spec.setLineType(-1);

		if (lineWidthBox.isSelected()) {
			try {
				spec.setLineWidth(Integer.parseInt(lineWidthField.getText()));
			} catch (Exception ex) {
				spec.setLineWidth(-1);
			}
		} else
			spec.setLineWidth(-1);
	}

	protected void updateGUI() {
		colorBox.setSelected(spec.getLinkColor() != null);
		if (spec.getLinkColor() != null)
			colorButton.setBackground(spec.getLinkColor());

		linkTypeBox.setSelected(spec.getLineType() != -1);
		if (spec.getLineType() != -1)
			lineTypeSelector.setSelectedIndex(spec.getLineType());

		lineWidthBox.setSelected(spec.getLineWidth() != -1);
		if (spec.getLineWidth() != -1)
			lineWidthField.setText(spec.getLineWidth() + "");
		updatePanels();
	}

	public void setSpec(RenderSpec spec) {
		if (spec != null)
			this.spec = (LinkRenderSpec) spec.clone();
		else
			this.spec = new LinkRenderSpec();
		updateGUI();
	}
}
