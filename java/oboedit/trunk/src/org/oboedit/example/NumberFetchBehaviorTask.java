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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.swing.ColorUtil;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.connect.GODBConnect;
import org.oboedit.graph.AbstractFetchTask;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.HTMLSpecField;
import org.oboedit.gui.filter.HeatmapColor;

import sun.rmi.runtime.GetThreadPoolAction;

public class NumberFetchBehaviorTask extends AbstractFetchTask<Integer> {

	protected GODBConnect gdbc = new GODBConnect();
	
	
	public static class AnnotationNumberFetchTaskConfiguration extends
			FetchTaskConfiguration {

		protected Color minColor;
		protected Color maxColor;

		public AnnotationNumberFetchTaskConfiguration() {
		}

		public AnnotationNumberFetchTaskConfiguration(boolean enabled,
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

	protected JTextArea sqlText = new JTextArea();
	protected JTextField hostField = new JTextField();
	protected JTextField dbField = new JTextField();
	protected JTextField userField = new JTextField();
	protected JTextField passwordField = new JTextField();
	protected JButton reconnectButton = new JButton();

	public NumberFetchBehaviorTask() {

		super(Integer.class);

		gdbc.connect();

		setEnabled(false);
	
		reconnectButton.setBorderPainted(true);
		reconnectButton.add(new JLabel("Reconnect"));
		reconnectButton.addActionListener(new ActionListener() {	
			
			public void actionPerformed(ActionEvent e) {
				
				gdbc.setSQL(sqlText.getText());
				gdbc.setHost(hostField.getText());
				gdbc.setDB(dbField.getText());
				gdbc.setUser(userField.getText());
				gdbc.setPassword(passwordField.getText());
				
				//
				//clearCache();
				
				System.err.println("RECONNECT:");
				System.err.println("\tSQL: " + gdbc.getSQL());
				System.err.println("\tHOST: " + gdbc.getHost());
				System.err.println("\tDB: " + gdbc.getDB());
				System.err.println("\tUSER: " + gdbc.getUser());
				System.err.println("\tPASSWORD: " + gdbc.getPassword());
			}
		});
		
		minColorButton.setBorderPainted(false);
		minColorButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				Color c = JColorChooser.showDialog(minColorButton,
						"Select min color", getMinColor());
				if (c != null)
					setMinColor(c);
			}
		});
		
		maxColorButton.setBorderPainted(false);
		maxColorButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				Color c = JColorChooser.showDialog(maxColorButton,
						"Select max color", getMaxColor());
				if (c != null)
					setMaxColor(c);
			}
		});
		setMinColor(Color.white);
		setMaxColor(Color.red);
	}

	@Override
	protected String getBehaviorID() {
		return "number_fetch";
	}

	@Override
	protected Integer getValue(IdentifiedObject io) {
		int num = -1; 
		num = gdbc.getCount( io.getID() );
		//System.err.println("\t___ SQL_COMPLETE");
		return num;
	}

	@Override
	protected GeneralRendererSpec getFetchedRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=\"black\">count: $"
						+ valueVar + "$</font></center>",
				BackgroundColorSpecField.FIELD, new HeatmapColor(getMinColor(),
						getMaxColor(), valueVar));
	}

	@Override
	protected GeneralRendererSpec getPendingRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=\"black\"><i>Loading...</i></font></center>");
	}

	@Override
	protected JComponent getConfigurationPanel() {

		// Listen on 
		enabledCheckBox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				setEnabled(enabledCheckBox.isSelected());
			}
		});
		
		// Assemble panels.
		JPanel configPanel = new JPanel();
		JPanel leftPanel = new JPanel();
		JPanel rightPanel = new JPanel();

		sqlText.setColumns(20);
        sqlText.setLineWrap(true);
        sqlText.setRows(5);
        sqlText.setWrapStyleWord(true);
        sqlText.setEditable(true);
        dbField.setColumns(20);
        
		configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.X_AXIS));
		leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
		rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
		
		rightPanel.add(enabledCheckBox);

		sqlText.setText( gdbc.getSQL());
		Box sqlPanel = Box.createHorizontalBox();
		sqlPanel.add(new JLabel("SQL text:"));
		sqlPanel.add(Box.createHorizontalStrut(10));
		sqlPanel.add(sqlText);
		leftPanel.add(sqlPanel);
		
		dbField.setText( gdbc.getDB());
		Box dbFieldPanel = Box.createHorizontalBox();
		dbFieldPanel.add(new JLabel("Database: "));
		dbFieldPanel.add(Box.createHorizontalStrut(10));
		dbFieldPanel.add(dbField);
		leftPanel.add(dbFieldPanel);
		
		hostField.setText( gdbc.getHost());
		Box hostFieldPanel = Box.createHorizontalBox();
		hostFieldPanel.add(new JLabel("Host: "));
		hostFieldPanel.add(Box.createHorizontalStrut(10));
		hostFieldPanel.add(hostField);
		leftPanel.add(hostFieldPanel);
		
		userField.setText( gdbc.getUser());
		Box userFieldPanel = Box.createHorizontalBox();
		userFieldPanel.add(new JLabel("User: "));
		userFieldPanel.add(Box.createHorizontalStrut(10));
		userFieldPanel.add(userField);
		leftPanel.add(userFieldPanel);
		
		passwordField.setText( gdbc.getPassword());
		Box passwordFieldPanel = Box.createHorizontalBox();
		passwordFieldPanel.add(new JLabel("Password: "));
		passwordFieldPanel.add(Box.createHorizontalStrut(10));
		passwordFieldPanel.add(passwordField);
		leftPanel.add(passwordFieldPanel);
		
		Box reconnectPanel = Box.createHorizontalBox();
		reconnectPanel.add(Box.createHorizontalStrut(10));
		reconnectPanel.add(reconnectButton);
		leftPanel.add(reconnectPanel);
		
		Box minPanel = Box.createHorizontalBox();
		minPanel.add(new JLabel("Minimum value color"));
		minPanel.add(Box.createHorizontalStrut(10));
		minPanel.add(minColorButton);
		rightPanel.add(minPanel);

		Box maxPanel = Box.createHorizontalBox();
		maxPanel.add(new JLabel("Maximum value color"));
		maxPanel.add(Box.createHorizontalStrut(10));
		maxPanel.add(maxColorButton);
		rightPanel.add(maxPanel);
		
		configPanel.add(leftPanel);
		configPanel.add(rightPanel);
		return configPanel;
	}

	@Override
	public Object getConfiguration() {
		return new AnnotationNumberFetchTaskConfiguration(isEnabled(), getMinColor(),
				getMaxColor());
	}

	@Override
	public void setConfiguration(Object config) {
		super.setConfiguration(config);
		if (config instanceof AnnotationNumberFetchTaskConfiguration) {
			AnnotationNumberFetchTaskConfiguration myConfig = (AnnotationNumberFetchTaskConfiguration) config;
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
		return "Number Renderer Plugin";
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
