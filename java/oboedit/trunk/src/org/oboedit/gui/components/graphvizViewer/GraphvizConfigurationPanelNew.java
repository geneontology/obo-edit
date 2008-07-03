package org.oboedit.gui.components.graphvizViewer;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.ListEditor;
import org.bbop.swing.widget.FontChooser;
import org.obo.datamodel.Link;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.components.graphvizViewer.*;



public class GraphvizConfigurationPanelNew extends ConfigurationPanel {

	protected GraphvizCanvas graphvizCanvasInstance;

	private static final long serialVersionUID = 1L;

	private JPanel panel = new JPanel();
	private JTabbedPane optionsPane = new JTabbedPane();
	protected Object[] shapeArr = { "box", "ellipse", "egg", "triangle",
			"diamond", "parallelogram", "house", "pentagon", "hexagon",
			"septagon", "octagon", "invtriangle" };
	protected JComboBox nodeShapeList = new JComboBox(shapeArr);
	protected JComboBox typeShapeList = new JComboBox(shapeArr);
	protected JComboBox obsoleteShapeList = new JComboBox(shapeArr);
	protected JCheckBox flipoverBox = new JCheckBox("Draw graph with root on top");
	protected JCheckBox showIDsBox = new JCheckBox("Show ids");
	protected static final String[] formatList = { "jpg", "gif" };
	JComboBox formatBox = new JComboBox(formatList);
	JPanel outerFontPanel = new JPanel();
	JPanel outerShapePanel = new JPanel();
	JPanel outerAppPanel = new JPanel();
	JPanel fontPanel = new JPanel();
	JPanel shapePanel = new JPanel();
	JPanel appPanel = new JPanel();
	TitledBorder linkFontBorder = new TitledBorder("Relationship type font");
	TitledBorder nodeFontBorder = new TitledBorder("Term name font");
	JPanel nodeShapePanel = new JPanel();
	TitledBorder nodeBorder = new TitledBorder("Term shape");
	TitledBorder obsoleteBorder = new TitledBorder("Obsolete shape");
	TitledBorder typeBorder = new TitledBorder("Type shape");
	JPanel obsoleteShapePanel = new JPanel();
	JPanel typeShapePanel = new JPanel();
	JTextArea messageArea = new JTextArea(
			"This should contain the path to the \"dot\" or "
			+ "\"dot.exe\" file included with the GraphViz "
			+ "software package. The package can be obtained "
			+ "from " + "http://www.research.att.com/sw/"
			+ "tools/graphviz/download.html", 3, 20);
	JPanel viewerFormatLine = new JPanel();
	JLabel appLabel = new JLabel("Application path");
	Box horzBox = new Box(BoxLayout.X_AXIS);
	JButton browseButton = new JButton("Browse...");
	JPanel appLine = new JPanel();
	JLabel formatLabel = new JLabel("Default display format");
	JLabel noTypeLabel = new JLabel("no type selected");
	final FontChooser linkFontChooser = new FontChooser();
	final FontChooser nodeFontChooser = new FontChooser();
	protected GraphvizConfigurableAttributes configurationConstructorsInstance = new GraphvizConfigurableAttributes();
	final JTextField appPathField = new JTextField(configurationConstructorsInstance.getDotPath());
	final ListEditor typeColorList = new ListEditor(new ColorEditor(),
			noTypeLabel, new Vector(), true, true, false, true, false);
	final JDialog pane = new JDialog((Frame) null, true);
	protected JPanel backgroundColorPanel = new JPanel();
	protected JColorChooser backgroundColorChooser = new JColorChooser();
	Vector data = configurationConstructorsInstance.getNamedColorList();



	/**
	 * @param graphvizCanvasInstance
	 */
	public GraphvizConfigurationPanelNew(GraphvizCanvas graphvizCanvasInstance) {
		setLayout(new BorderLayout());

		//not sure what this line does.
		pane.setContentPane(panel);

//		Adds the main panel to the configuration window.

		add(panel);
		panel.setBackground(Preferences.defaultBackgroundColor());
		panel.setLayout(new BorderLayout());

//		Adds JTabbedpane optionsPane to main panel. 

		panel.add(optionsPane, "Center");

//		Adds the tabs to the JTabbedpane.

		optionsPane.removeAll();
		optionsPane.addTab("Fonts", outerFontPanel);
		optionsPane.addTab("Colors", typeColorList);
		optionsPane.addTab("Shapes", outerShapePanel);
		optionsPane.addTab("Behavior", outerAppPanel);

//		Set tab panel characteristics characteristics.	

		outerShapePanel.setBackground(Preferences.defaultBackgroundColor());
		outerShapePanel.setLayout(new BorderLayout());	

		outerAppPanel.setBackground(Preferences.defaultBackgroundColor());
		outerAppPanel.setLayout(new BorderLayout());



//		Add constituent parts of outerFontPanel.

		outerFontPanel.setBackground(Preferences.defaultBackgroundColor());
		outerFontPanel.setLayout(new BorderLayout());

//		Adds fontPanel.

		outerFontPanel.add(fontPanel, "North");
		fontPanel.setOpaque(false);
		fontPanel.setLayout(new BoxLayout(fontPanel, BoxLayout.Y_AXIS));

//		Adds linkFontChoose to fontPanel.

		fontPanel.add(linkFontChooser);
		linkFontChooser.setBorder(linkFontBorder);
		linkFontChooser.setOpaque(false);

//		Adds nodeFontChooser to fontPanel.

		fontPanel.add(nodeFontChooser);
		nodeFontChooser.setBorder(nodeFontBorder);
		nodeFontChooser.setOpaque(false);


//		Configure typeColorList.
		typeColorList.setData(data);
		typeColorList.setVectorEditable(false);


//		Add constituent parts of outerShapePanel (which is a tab).

//		Add shapePanel to outerShapePanel.

		outerShapePanel.add(shapePanel, "North");
		shapePanel.setBackground(Preferences.defaultBackgroundColor());
		shapePanel.setLayout(new BoxLayout(shapePanel, BoxLayout.Y_AXIS));

//		Add nodeShapePanel to shapePanel.		

		shapePanel.add(nodeShapePanel);
		shapePanel.add(Box.createVerticalStrut(5));
		nodeShapePanel.setOpaque(false);
		nodeShapePanel.setLayout(new BorderLayout());
		nodeShapePanel.setBorder(nodeBorder);
		nodeBorder.setTitleFont(getFont());

//		Add nodeShapeList to nodeShapePanel.		

		nodeShapePanel.add(nodeShapeList, "Center");
		nodeShapeList.setBackground(Preferences.defaultButtonColor());
		nodeShapeList.setFont(getFont());


//		Add obdoleteShapePanel to shapePanel.

		shapePanel.add(obsoleteShapePanel);
		shapePanel.add(Box.createVerticalStrut(5));
		obsoleteShapePanel.setOpaque(false);
		obsoleteShapePanel.setLayout(new BorderLayout());

//		Add obsoleteShapeList to obsoleteShapePanel.	

		obsoleteShapePanel.add(obsoleteShapeList, "Center");
		obsoleteShapeList.setBackground(Preferences.defaultButtonColor());
		obsoleteShapeList.setFont(getFont());
		obsoleteShapePanel.setBorder(obsoleteBorder);
		obsoleteBorder.setTitleFont(getFont());

//		Add typeShapePanel to shapePanel.		

		shapePanel.add(typeShapePanel);
		typeShapePanel.setOpaque(false);
		typeShapePanel.setLayout(new BorderLayout());
		typeShapePanel.setBorder(typeBorder);
		typeBorder.setTitleFont(getFont());

//		Add typeShapeList to typeShapePanel.

		typeShapePanel.add(typeShapeList, "Center");
		typeShapeList.setBackground(Preferences.defaultButtonColor());
		typeShapeList.setFont(getFont());

//		Add constituent parts of outerAppPanel (a tab).

//		Add appPanel to outerAppPanel.		

		outerAppPanel.add(appPanel, "North");
		appPanel.setBackground(Preferences.defaultBackgroundColor());
		appPanel.setLayout(new BoxLayout(appPanel, BoxLayout.Y_AXIS));
		appPanel.add(Box.createVerticalStrut(10));

//		Add viewerFormatLine to appPanel.	

		appPanel.add(viewerFormatLine);
		appPanel.add(Box.createVerticalStrut(10));
		viewerFormatLine.setOpaque(false);
		viewerFormatLine.setLayout(new BorderLayout());

//		Add formatLabel to viewerFormatLine.		

		viewerFormatLine.add(formatLabel, "West");
		viewerFormatLine.add(Box.createHorizontalStrut(5));
		formatLabel.setFont(getFont());

//		Add formatBox to viewerFormatLine.		

		viewerFormatLine.add(formatBox);



//		Add flipoverBox to appPanel.	

		appPanel.add(flipoverBox);
		appPanel.add(Box.createVerticalStrut(10));

//		Add showIDBox to appPanel.		

		appPanel.add(showIDsBox);
		appPanel.add(Box.createVerticalStrut(10));

//		Add appLine to appPanel.	

		appPanel.add(appLine);
		appPanel.add(Box.createVerticalStrut(10));
		appLine.setLayout(new BorderLayout());

//		Add appLabel to appLine. 		

		appLine.add(appLabel, "West");
		appLabel.setFont(getFont());

//		Add horzBox to appLine.		

		appLine.add(horzBox, "Center");
		horzBox.add(Box.createHorizontalStrut(5));

//		Add appPathField to horzBox.

		horzBox.add(appPathField);
		horzBox.add(Box.createHorizontalStrut(5));

//		Add BrowseButton to appLine.		

		appLine.add(browseButton, "East");
		appLine.setOpaque(false);
		browseButton.setFont(Preferences.getPreferences().getFont());
		browseButton.setBackground(Preferences.defaultButtonColor());

//		Add messageArea to appPanel.	

		appPanel.add(messageArea);
		messageArea.setEditable(false);
		messageArea.setBorder(null);
		messageArea.setOpaque(false);
		messageArea.setLineWrap(true);
		messageArea.setWrapStyleWord(true);

//		Code to add actionListeners.		

		browseButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				browseButtonActionPerformed(evt);
			}
		});


		this.graphvizCanvasInstance = graphvizCanvasInstance;
	}

	/**
	 * @param evt
	 */
	protected void browseButtonActionPerformed(java.awt.event.ActionEvent evt) {
		JFileChooser chooser = new JFileChooser();
		if (chooser.showOpenDialog(graphvizCanvasInstance) == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			File macPath = new File(file, "Contents/MacOS/dot");
			if (!file.getName().equals("dot") && file.isDirectory()
					&& macPath.exists()) {
				appPathField.setText(macPath.toString());
			} else
				appPathField.setText(file.toString());
		}
	};



	@Override
	public void commit() {
		System.out.println("GraphvizConfigurationPanelNew: commit() run.");
		configurationConstructorsInstance.setDotPath(appPathField.getText());
		configurationConstructorsInstance.setLabelFont(linkFontChooser.getChosenFont());
		configurationConstructorsInstance.setNodeFont(nodeFontChooser.getChosenFont());
		configurationConstructorsInstance.setTypeShape((String) typeShapeList.getSelectedItem());
		configurationConstructorsInstance.setNodeShape((String) nodeShapeList.getSelectedItem());
		configurationConstructorsInstance.setObsoleteShape((String) obsoleteShapeList.getSelectedItem());
		configurationConstructorsInstance.setFlipOver(flipoverBox.isSelected());
		configurationConstructorsInstance.setShowIDs(showIDsBox.isSelected());

		for (int i = 0; i < typeColorList.getData().size(); i++) {
			Object o = typeColorList.getData().get(i);
			if (o instanceof TypeColorPair) {
				TypeColorPair tc = (TypeColorPair) o;
				configurationConstructorsInstance.getColorMap().put(tc.getTypeID(), tc.getPair());
			} else if (o instanceof NamedColor) {
				NamedColor nc = (NamedColor) o;
				configurationConstructorsInstance.setNamedColor(nc.getName(), nc.getColor());
			}
		}

		configurationConstructorsInstance.setViewerFormat((String) formatBox.getSelectedItem());

		graphvizCanvasInstance.reloadImage();


	}
	
	protected ColorPair getColor(Link tr) {
		ColorPair c = (ColorPair) configurationConstructorsInstance.getColorMap().get(
				tr.getType().getID());
		if (c == null)
			c = GraphvizCanvas.defaultLabelColors;
		// if (TermUtil.isRedundant(tr)) {
		// c = defaultRedundantColors;
		// }
		return c;
	}

	@Override
	public void init() {
		System.out.println("GraphvizConfigurationPanelNew: init() run.");

		//I'm not sure if any of the lines below are needed. 
		//The graphviz path works without them and they
		//don't make the background color work. 
		configurationConstructorsInstance.getDotPath();
		configurationConstructorsInstance.getLabelFont();
		configurationConstructorsInstance.getNodeFont();
		configurationConstructorsInstance.getTypeShape();
		configurationConstructorsInstance.getNodeShape();
		configurationConstructorsInstance.getObsoleteShape();
		configurationConstructorsInstance.getFlipOver();
		configurationConstructorsInstance.getShowIDs();

		//what does this bit do?
		//I think this bit may be why I cannot change the black background. 
		for (int i = 0; i < typeColorList.getData().size(); i++) {
			Object o = typeColorList.getData().get(i);
			if (o instanceof TypeColorPair) {
				TypeColorPair tc = (TypeColorPair) o;
				configurationConstructorsInstance.getColorMap().put(tc.getTypeID(), tc.getPair());
			} else if (o instanceof NamedColor) {
				NamedColor nc = (NamedColor) o;
				configurationConstructorsInstance.setNamedColor(nc.getName(), nc.getColor());
			}
		}
		
		configurationConstructorsInstance.getViewerFormat();

		//confusion ends here. 
		
		

	}


	@Override
	public GUIComponent getComponent() {
		System.out.println("Config panel New : getComponent.");
		return graphvizCanvasInstance;
	}	

	@Override
	public void setComponent(GUIComponent comp) {
		if (comp instanceof GraphvizCanvas) {
			graphvizCanvasInstance = (GraphvizCanvas)comp;
			System.out.println("Config panel New : setComponent.");
		}
	}
}

