package org.oboedit.gui.widget;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import org.obo.filters.*;
import org.oboedit.gui.filter.ObjectRenderSpec;
import org.oboedit.gui.filter.RenderSpec;
import org.oboedit.gui.filter.SpecEditor;

public class ObjectSpecEditor extends JPanel implements SpecEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected JCheckBox colorBox = new JCheckBox();
	protected JCheckBox underlineBox = new JCheckBox();
	protected JCheckBox fontBox = new JCheckBox();
	protected JCheckBox sizeBox = new JCheckBox();
	protected JCheckBox boldBox = new JCheckBox();
	protected JCheckBox italicBox = new JCheckBox();

	protected JLabel colorLabel = new JLabel("Change color", JLabel.TRAILING);
	protected JLabel fontLabel = new JLabel("Change font", JLabel.TRAILING);
	protected JLabel sizeLabel = new JLabel("Change size", JLabel.TRAILING);
	protected JLabel boldLabel = new JLabel("Make bold", JLabel.TRAILING);
	protected JLabel italicLabel = new JLabel("Make italic", JLabel.TRAILING);
	protected JLabel underlineLabel = new JLabel("Make underlined",
			JLabel.TRAILING);

	protected JPanel colorPanel = new JPanel();
	protected JPanel fontPanel = new JPanel();
	protected JPanel sizePanel = new JPanel();
	protected JPanel boldPanel = new JPanel();
	protected JPanel italicPanel = new JPanel();
	protected JPanel underlinePanel = new JPanel();

	protected JButton colorButton = new JButton();
	protected JComboBox fontListBox = new JComboBox();
	protected JTextField sizeField = new JTextField(3);

	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		colorBox.setEnabled(enabled);
		underlineBox.setEnabled(enabled);
		fontBox.setEnabled(enabled);
		sizeBox.setEnabled(enabled);
		boldBox.setEnabled(enabled);
		italicBox.setEnabled(enabled);

		colorLabel.setEnabled(enabled);
		fontLabel.setEnabled(enabled);
		sizeLabel.setEnabled(enabled);
		boldLabel.setEnabled(enabled);
		italicLabel.setEnabled(enabled);
		underlineLabel.setEnabled(enabled);
		colorButton.setEnabled(enabled);
		fontListBox.setEnabled(enabled);
		sizeField.setEnabled(enabled);
	}

	public ObjectSpecEditor() {
		super();

		buildGUI();
		attachListeners();
		loadFontList();
	}

	protected class FontListCellRenderer extends DefaultListCellRenderer {
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

	public RenderSpec createNewSpec() {
		return new ObjectRenderSpec();
	}

	protected void loadFontList() {
		GraphicsEnvironment gEnv = GraphicsEnvironment
				.getLocalGraphicsEnvironment();
		String[] fontNames = gEnv.getAvailableFontFamilyNames();
		for (int i = 0; i < fontNames.length; i++)
			fontListBox.addItem(fontNames[i]);
	}

	protected void attachListeners() {
		fontListBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fontListBox.setFont(new Font((String) fontListBox
						.getSelectedItem(), 0, getFont().getSize()));
			}
		});
		colorBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				refreshGUI();
			}
		});
		fontBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				refreshGUI();
			}
		});
		sizeBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				refreshGUI();
			}
		});
		colorButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Color color = JColorChooser.showDialog(ObjectSpecEditor.this,
						"Choose a color", colorButton.getBackground());
				colorButton.setBackground(color);
			}
		});
	}

	public void setButtonColor(Color buttonColor) {
		fontListBox.setBackground(buttonColor);
		colorBox.setBackground(buttonColor);
		underlineBox.setBackground(buttonColor);
		fontBox.setBackground(buttonColor);
		sizeBox.setBackground(buttonColor);
		boldBox.setBackground(buttonColor);
		italicBox.setBackground(buttonColor);
	}

	protected void refreshGUI() {
		if (colorBox.isSelected()) {
			colorPanel.add(colorButton);
			colorPanel.revalidate();
		} else {
			colorPanel.remove(colorButton);
			colorPanel.revalidate();
		}

		if (fontBox.isSelected()) {
			fontPanel.add(fontListBox);
			fontPanel.validate();
		} else {
			fontPanel.remove(fontListBox);
			fontPanel.validate();
		}

		if (sizeBox.isSelected()) {
			sizePanel.add(sizeField);
			sizePanel.validate();
		} else {
			sizePanel.remove(sizeField);
			sizePanel.validate();
		}

		revalidate();
		repaint();

	}

	public void setSpec(RenderSpec s) {
		ObjectRenderSpec spec;
		if (s != null)
			spec = (ObjectRenderSpec) s.clone();
		else
			spec = new ObjectRenderSpec();
		if (spec.getForegroundColor() != null) {
			colorButton.setBackground(spec.getForegroundColor());
			colorBox.setSelected(true);
		} else {
			colorButton.setBackground(Color.red);
			colorBox.setSelected(false);
		}

		if (spec.getFontName() != null) {
			fontListBox.setSelectedItem(spec.getFontName());
			fontBox.setSelected(true);
		} else {
			fontListBox.setSelectedIndex(0);
			fontBox.setSelected(false);
		}

		if (spec.getFontSize() != -1) {
			sizeField.setText(spec.getFontSize() + "");
			sizeBox.setSelected(true);
		} else {
			sizeField.setText(getFont().getSize() + "");
			sizeBox.setSelected(false);
		}

		boldBox.setSelected(spec.getBold());
		italicBox.setSelected(spec.getItalic());
		underlineBox.setSelected(spec.getUnderlined());
		refreshGUI();
	}

	public RenderSpec getSpec() {
		ObjectRenderSpec spec = new ObjectRenderSpec();
		spec.setUnderlined(underlineBox.isSelected());
		spec.setBold(boldBox.isSelected());
		spec.setItalic(italicBox.isSelected());
		if (fontBox.isSelected())
			spec.setFontName((String) fontListBox.getSelectedItem());
		else
			spec.setFontName(null);
		if (colorBox.isSelected())
			spec.setForegroundColor(colorButton.getBackground());
		else
			spec.setForegroundColor(null);
		if (sizeBox.isSelected()) {
			try {
				spec.setFontSize(Integer.parseInt(sizeField.getText()));
			} catch (NumberFormatException ex) {
			}
		} else
			spec.setFontSize(-1);
		return spec;
	}

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (fontListBox != null) {
			colorLabel.setFont(font);
			fontLabel.setFont(font);
			sizeLabel.setFont(font);
			boldLabel.setFont(font);
			italicLabel.setFont(font);
			fontListBox.setFont(font);
			sizeField.setFont(font);
			underlineLabel.setFont(font);
		}
	}

	protected int getLineHeight() {
		return getFont().getSize() * 2;
	}

	protected void buildGUI() {
		setOpaque(false);
		colorButton.setBorderPainted(false);
		fontListBox.setRenderer(new FontListCellRenderer());
		colorButton.setBackground(Color.red);

		colorBox.setOpaque(false);
		fontBox.setOpaque(false);
		sizeBox.setOpaque(false);
		boldBox.setOpaque(false);
		italicBox.setOpaque(false);
		underlineBox.setOpaque(false);

		colorPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		fontPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		sizePanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		boldPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		italicPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));
		underlinePanel.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				getLineHeight()));

		colorPanel.setOpaque(false);
		fontPanel.setOpaque(false);
		sizePanel.setOpaque(false);
		boldPanel.setOpaque(false);
		italicPanel.setOpaque(false);
		underlinePanel.setOpaque(false);

		colorPanel.setLayout(new BoxLayout(colorPanel, BoxLayout.X_AXIS));
		fontPanel.setLayout(new BoxLayout(fontPanel, BoxLayout.X_AXIS));
		sizePanel.setLayout(new BoxLayout(sizePanel, BoxLayout.X_AXIS));
		boldPanel.setLayout(new BoxLayout(boldPanel, BoxLayout.X_AXIS));
		italicPanel.setLayout(new BoxLayout(italicPanel, BoxLayout.X_AXIS));
		underlinePanel
				.setLayout(new BoxLayout(underlinePanel, BoxLayout.X_AXIS));

		colorPanel.add(colorLabel);
		colorPanel.add(Box.createHorizontalStrut(10));
		colorPanel.add(colorBox);
		colorPanel.add(Box.createHorizontalStrut(10));
		colorPanel.add(Box.createHorizontalGlue());

		fontPanel.add(fontLabel);
		fontPanel.add(Box.createHorizontalStrut(10));
		fontPanel.add(fontBox);
		fontPanel.add(Box.createHorizontalStrut(10));
		fontPanel.add(Box.createHorizontalGlue());

		sizePanel.add(sizeLabel);
		sizePanel.add(Box.createHorizontalStrut(10));
		sizePanel.add(sizeBox);
		sizePanel.add(Box.createHorizontalStrut(10));
		sizePanel.add(Box.createHorizontalGlue());

		boldPanel.add(boldLabel);
		boldPanel.add(Box.createHorizontalStrut(10));
		boldPanel.add(boldBox);
		boldPanel.add(Box.createHorizontalStrut(10));
		boldPanel.add(Box.createHorizontalGlue());

		italicPanel.add(italicLabel);
		italicPanel.add(Box.createHorizontalStrut(10));
		italicPanel.add(italicBox);
		italicPanel.add(Box.createHorizontalStrut(10));
		italicPanel.add(Box.createHorizontalGlue());

		underlinePanel.add(underlineLabel);
		underlinePanel.add(Box.createHorizontalStrut(10));
		underlinePanel.add(underlineBox);
		underlinePanel.add(Box.createHorizontalStrut(10));
		underlinePanel.add(Box.createHorizontalGlue());

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(colorPanel);
		add(fontPanel);
		add(sizePanel);
		add(boldPanel);
		add(italicPanel);
		add(underlinePanel);
		add(Box.createVerticalGlue());
	}
}
