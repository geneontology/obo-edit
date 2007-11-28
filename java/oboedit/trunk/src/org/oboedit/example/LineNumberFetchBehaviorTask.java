package org.oboedit.example;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.swing.ColorUtil;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.graph.AbstractFetchTask;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.HTMLSpecField;
import org.oboedit.gui.filter.HeatmapColor;

public class LineNumberFetchBehaviorTask extends AbstractFetchTask<Integer> {

	public static class LineNumberFetchTaskConfiguration extends
			FetchTaskConfiguration {
		protected Color minColor;
		protected Color maxColor;

		public LineNumberFetchTaskConfiguration() {
		}

		public LineNumberFetchTaskConfiguration(boolean enabled,
				Color minColor, Color maxColor) {
			setEnabled(enabled);
			setMinColor(minColor);
			setMaxColor(maxColor);
		}

		public Color getMinColor() {
			return minColor;
		}

		public void setMinColor(Color minColor) {
			this.minColor = minColor;
		}

		public Color getMaxColor() {
			return maxColor;
		}

		public void setMaxColor(Color maxColor) {
			this.maxColor = maxColor;
		}
	}

	protected Color minColor;
	protected Color maxColor;

	protected JCheckBox enabledCheckBox = new JCheckBox("Enabled", true);
	protected JButton minColorButton = new JButton();
	protected JButton maxColorButton = new JButton();

	public LineNumberFetchBehaviorTask() {
		super(Integer.class);
		setEnabled(false);
		minColorButton.setBorderPainted(false);
		maxColorButton.setBorderPainted(false);
		minColorButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				Color c = JColorChooser.showDialog(minColorButton,
						"Select min color", getMinColor());
				if (c != null)
					setMinColor(c);
			}

		});
		maxColorButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				Color c = JColorChooser.showDialog(maxColorButton,
						"Select max color", getMaxColor());
				if (c != null)
					setMaxColor(c);
			}

		});
		setMinColor(Color.yellow);
		setMaxColor(Color.red);
	}

	@Override
	protected String getBehaviorID() {
		return "line_number_fetch";
	}

	protected String fileLoc = "/Users/jrichter/ontology/gene_ontology_edit.obo";

	@Override
	protected Integer getValue(IdentifiedObject io) {
		int lineNum = 0;
		boolean found = false;
		try {
			BufferedReader reader = new BufferedReader(new FileReader(fileLoc));
			String line;
			while ((line = reader.readLine()) != null) {
				lineNum++;
				if (line.startsWith("id: " + io.getID())) {
					found = true;
					break;
				}
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (!found)
			lineNum = -1;
		return lineNum;
	}

	@Override
	protected GeneralRendererSpec getFetchedRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=white>defined on line $"
						+ valueVar + "$</font></center>",
				BackgroundColorSpecField.FIELD, new HeatmapColor(getMinColor(),
						getMaxColor(), valueVar));
	}

	@Override
	protected GeneralRendererSpec getPendingRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=white><i>Loading...</i></font></center>");
	}

	@Override
	protected JComponent getConfigurationPanel() {
		JPanel configPanel = new JPanel();
		enabledCheckBox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				setEnabled(enabledCheckBox.isSelected());
			}
		});
		configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.Y_AXIS));
		configPanel.add(enabledCheckBox);
		Box minPanel = Box.createHorizontalBox();
		minPanel.add(new JLabel("Minimum value color"));
		minPanel.add(Box.createHorizontalStrut(10));
		minPanel.add(minColorButton);
		configPanel.add(minPanel);
		Box maxPanel = Box.createHorizontalBox();
		maxPanel.add(new JLabel("Maximum value color"));
		maxPanel.add(Box.createHorizontalStrut(10));
		maxPanel.add(maxColorButton);
		configPanel.add(maxPanel);
		return configPanel;
	}

	@Override
	public Object getConfiguration() {
		return new LineNumberFetchTaskConfiguration(isEnabled(), getMinColor(),
				getMaxColor());
	}

	@Override
	public void setConfiguration(Object config) {
		super.setConfiguration(config);
		if (config instanceof LineNumberFetchTaskConfiguration) {
			LineNumberFetchTaskConfiguration myConfig = (LineNumberFetchTaskConfiguration) config;
			setMinColor(myConfig.getMinColor());
			setMaxColor(myConfig.getMaxColor());
		}
	}
	
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		enabledCheckBox.setSelected(enabled);
	}

	@Override
	protected String getName() {
		return "Line Number Renderer Plugin";
	}

	public Color getMinColor() {
		return minColor;
	}

	public void setMinColor(Color minColor) {
		this.minColor = minColor;
		minColorButton.setBackground(minColor);
		minColorButton.setText(ColorUtil.getName(minColor));
		resetRenderers();
	}

	public Color getMaxColor() {
		return maxColor;
	}

	public void setMaxColor(Color maxColor) {
		this.maxColor = maxColor;
		maxColorButton.setBackground(maxColor);
		maxColorButton.setText(ColorUtil.getName(maxColor));
		resetRenderers();
	}
}
