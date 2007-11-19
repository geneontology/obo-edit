package org.oboedit.gui.filter;

import info.clearthought.layout.TableLayout;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.plaf.basic.BasicBorders.RadioButtonBorder;

import org.bbop.swing.SwingUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.Relationship;
import org.obo.filters.SearchCriterion;
import org.oboedit.controller.FilterManager;

public class ColorSpecEditor extends JPanel implements
		GeneralRendererSpecFieldEditor<ColorProvider> {

	protected static final String HEATMAP_KEY = "HEATMAP";
	protected static final String COLOR_KEY = "COLOR";

	protected static class ColorButtonListener implements ActionListener {
		protected JButton colorButton;

		public ColorButtonListener(JButton colorButton) {
			this.colorButton = colorButton;
		}

		public void actionPerformed(ActionEvent e) {
			Color color = JColorChooser.showDialog(colorButton,
					"Choose a color", colorButton.getBackground());
			if (color != null)
				colorButton.setBackground(color);
		}
	}

	protected JCheckBox blendBox = new JCheckBox("Do blending");
	protected JButton colorButton = new JButton(" ");

	protected JButton minColorButton = new JButton(" ");
	protected JButton maxColorButton = new JButton(" ");
	protected JButton noColorButton = new JButton(" ");

	protected JComboBox minComboBox = new JComboBox();
	protected JComboBox maxComboBox = new JComboBox();
	protected JTextField minField = new JTextField();
	protected JTextField maxField = new JTextField();

	protected JPanel minValuePanel = new JPanel();
	protected JPanel maxValuePanel = new JPanel();

	protected JRadioButton simpleButton = new JRadioButton("Simple coloring");
	protected JRadioButton heatmapButton = new JRadioButton("Heat map coloring");
	protected JComboBox criteriaBox = new JComboBox();
	protected JPanel centerPanel = new JPanel();
	
	protected JPanel colorPanel;
	protected JPanel heatmapPanel;

	public ColorSpecEditor() {
		colorButton.addActionListener(new ColorButtonListener(colorButton));
		colorButton.setBorderPainted(false);
		colorButton.setBackground(Color.red);
		setLayout(new BorderLayout());
		JPanel southPanel = new JPanel();
		southPanel.setLayout(new BoxLayout(southPanel, BoxLayout.X_AXIS));
		ButtonGroup group = new ButtonGroup();
		group.add(simpleButton);
		group.add(heatmapButton);
		southPanel.add(blendBox);
		southPanel.add(simpleButton);
		southPanel.add(heatmapButton);

		centerPanel.setLayout(new GridLayout(1,1));
		colorPanel = new JPanel();
		colorPanel.setLayout(new GridLayout(1, 1));
		colorPanel.add(colorButton);

		centerPanel.add(colorPanel);
		heatmapPanel = new JPanel();
		double[] rowSizes = new double[6];
		Arrays.fill(rowSizes, TableLayout.PREFERRED);
		double[][] sizes = { { TableLayout.PREFERRED, TableLayout.FILL },
				rowSizes };
		heatmapPanel.setLayout(new TableLayout(sizes));

		minValuePanel.setLayout(new BoxLayout(minValuePanel, BoxLayout.X_AXIS));
		minValuePanel.add(minComboBox);
		minValuePanel.add(Box.createHorizontalStrut(5));
		minValuePanel.add(minField);
		minValuePanel.add(Box.createHorizontalGlue());
		maxValuePanel.setLayout(new BoxLayout(maxValuePanel, BoxLayout.X_AXIS));
		maxValuePanel.add(maxComboBox);
		maxValuePanel.add(Box.createHorizontalStrut(5));
		maxValuePanel.add(maxField);
		maxValuePanel.add(Box.createHorizontalGlue());

		heatmapPanel.add(new JLabel("Min color"), "0,0");
		heatmapPanel.add(minColorButton, "1,0");
		heatmapPanel.add(new JLabel("Max color"), "0,1");
		heatmapPanel.add(maxColorButton, "1,1");
		heatmapPanel.add(new JLabel("Blank color"), "0,2");
		heatmapPanel.add(noColorButton, "1,2");
		heatmapPanel.add(new JLabel("Criterion"), "0,3");
		heatmapPanel.add(criteriaBox, "1,3");
		heatmapPanel.add(new JLabel("Minimum Value"), "0,4");
		heatmapPanel.add(minValuePanel, "1,4");
		heatmapPanel.add(new JLabel("Maximum Value"), "0,5");
		heatmapPanel.add(maxValuePanel, "1,5");

		minComboBox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				minField.setVisible(minComboBox.getSelectedIndex() == 1);
			}

		});
		maxComboBox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				maxField.setVisible(maxComboBox.getSelectedIndex() == 1);
			}

		});

		minColorButton
				.addActionListener(new ColorButtonListener(minColorButton));
		maxColorButton
				.addActionListener(new ColorButtonListener(maxColorButton));
		noColorButton.addActionListener(new ColorButtonListener(noColorButton));
		minColorButton.setBorderPainted(false);
		maxColorButton.setBorderPainted(false);
		noColorButton.setBorderPainted(false);


		heatmapButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				setMode(HEATMAP_KEY);
			}

		});
		simpleButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				setMode(COLOR_KEY);
			}

		});

		add(southPanel, "South");
		add(centerPanel, "Center");
	}

	public void setMode(String mode) {
		centerPanel.removeAll();
		if (mode.equals(HEATMAP_KEY)) {
			heatmapButton.setSelected(true);
			centerPanel.add(heatmapPanel);
		} else {
			simpleButton.setSelected(true);
			centerPanel.add(colorPanel);
		}
		JComponent c = SwingUtil.getAncestorOfClass(GeneralSpecEditor.class, this);
		if (c != null) {
			c.validate();
			c.repaint();
		}
	}

	public void setValue(ColorProvider o) {
		if (o != null) {
			if (o instanceof ConfiguredColor) {
				ConfiguredColor c = (ConfiguredColor) o;
				colorButton.setBackground(c.getColor(null, null));
				setMode(COLOR_KEY);
			} else if (o instanceof HeatmapColor) {
				HeatmapColor c = (HeatmapColor) o;
				minColorButton.setBackground(c.getMinColor());
				maxColorButton.setBackground(c.getMaxColor());
				noColorButton.setBackground(c.getNoValueColor());
				String minVal = c.getMinValue();
				if (minVal.equals("$min$")) {
					minComboBox.setSelectedIndex(0);
				} else if (minVal.startsWith("$") && minVal.endsWith("$")) {
					SearchCriterion<?, ?> crit = FilterManager.getManager()
							.getCriterion(minVal);
					if (crit != null)
						minComboBox.setSelectedItem(crit);
				} else {
					minComboBox.setSelectedIndex(1);
					minField.setText(minVal);
				}
				String maxVal = c.getMaxValue();
				if (maxVal.equals("$min$")) {
					maxComboBox.setSelectedIndex(0);
				} else if (maxVal.startsWith("$") && maxVal.endsWith("$")) {
					SearchCriterion<?, ?> crit = FilterManager.getManager()
							.getCriterion(
									maxVal.substring(1, maxVal.length() - 1));
					if (crit != null)
						maxComboBox.setSelectedItem(crit);
				} else {
					maxComboBox.setSelectedIndex(1);
					maxField.setText(maxVal);
				}
				SearchCriterion<?, ?> crit = FilterManager.getManager()
						.getCriterion(c.getCriterionID());
				if (crit != null)
					criteriaBox.setSelectedItem(crit);
				setMode(HEATMAP_KEY);
			}
			blendBox.setSelected(o.isDoBlend());
		} else {
			colorButton.setBackground(Color.red);
			minColorButton.setBackground(Color.black);
			maxColorButton.setBackground(Color.red);
			noColorButton.setBackground(Color.white);
			blendBox.setSelected(false);
			setMode(COLOR_KEY);
			criteriaBox.removeAllItems();
			maxComboBox.removeAllItems();
			minComboBox.removeAllItems();
			minComboBox.addItem("<auto-calculate>");
			maxComboBox.addItem("<auto-calculate>");
			minComboBox.addItem("<enter constant>");
			maxComboBox.addItem("<enter constant>");
			for (SearchCriterion<?, ?> c : FilterManager.getManager()
					.getDisplayableCriteria()) {
				Class<?> returnType = c.getReturnType();
				if (!Number.class.isAssignableFrom(returnType))
					continue;
				if (IdentifiedObject.class.isAssignableFrom(c.getInputType())
						|| Relationship.class
								.isAssignableFrom(c.getInputType()))
					criteriaBox.addItem(c);
				if (OBOSession.class.isAssignableFrom(c.getInputType())) {
					minComboBox.addItem(c);
					maxComboBox.addItem(c);
				}
			}
			minField.setText("0");
			maxField.setText("10");
		}
	}

	public ColorProvider getValue() {
		if (simpleButton.isSelected())
			return new ConfiguredColor(colorButton.getBackground(), blendBox
					.isSelected());
		else if (heatmapButton.isSelected()) {
			String minValue;
			if (minComboBox.getSelectedIndex() == 0) {
				minValue = "$min$";
			} else if (minComboBox.getSelectedIndex() == 1) {
				minValue = minField.getText();
			} else {
				minValue = "$"
						+ ((SearchCriterion) minComboBox.getSelectedItem())
								.getID() + "$";
			}

			String maxValue;
			if (maxComboBox.getSelectedIndex() == 0) {
				maxValue = "$max$";
			} else if (maxComboBox.getSelectedIndex() == 1) {
				maxValue = maxField.getText();
			} else {
				maxValue = "$"
						+ ((SearchCriterion) maxComboBox.getSelectedItem())
								.getID() + "$";
			}
			return new HeatmapColor(minColorButton.getBackground(),
					maxColorButton.getBackground(), noColorButton
							.getBackground(), minValue, maxValue,
					((SearchCriterion) criteriaBox.getSelectedItem()).getID(),
					blendBox.isSelected());
		} else
			return null;
	}
}
