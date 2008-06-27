package org.oboedit.gui.components.graphvizViewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.TitledBorder;

import org.apache.log4j.Logger;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.swing.ListEditor;
import org.bbop.swing.widget.FontChooser;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.components.OntologyEditorConfigEditor;
import org.oboedit.gui.components.ConfigurableTextComponent.InfoConfig;
import org.oboedit.gui.components.GraphvizCanvas.GraphvizConfiguration;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;

public class GraphvizCanvas extends AbstractGUIComponent {

	private static final long serialVersionUID = 1L;

	// initialize logger
	protected final static Logger logger = Logger.getLogger(GraphvizCanvas.class);

	protected static final String TERM_TEXT_COLOR = "Term text color";
	protected static final String BACKGROUND_COLOR = "Background color";
	protected static final String TERM_BACKGROUND_COLOR = "Term background color";
	protected static final String TERM_STROKE_COLOR = "Term stroke color";
	protected static final String TYPE_STROKE_COLOR = "Type stroke color";
	protected static final String TYPE_BACKGROUND_COLOR = "Type background color";
	protected static final String TYPE_TEXT_COLOR = "Type text color";
	protected static final String OBSOLETE_STROKE_COLOR = "Obsolete stroke color";
	protected static final String OBSOLETE_BACKGROUND_COLOR = "Obsolete background color";
	protected static final String OBSOLETE_TEXT_COLOR = "Obsolete text color";
	protected static final String SELECTED_ONLY = "selected only";
	protected static final String SELECTED_TO_ROOT = "selected to root";
	protected static final String MINIMAL_SELECTION_GRAPH = "minimal connected graph";
	protected static final int MAX_LINE_LENGTH = 25;
	protected static final ColorPair defaultLabelColors = new ColorPair(Color.blue, Color.blue);
	protected static final ColorPair defaultRedundantColors = new ColorPair(Color.red, Color.red);
	protected static final String[] formatList = { "jpg", "gif" };
	protected String mode = SELECTED_TO_ROOT;
	static int idgen = 0;
	public JCheckBox primaryFiltersCheckbox = new JCheckBox("Use primary filters", false);
	protected GraphvizConfigurationOld graphvizConfigurationOldInstance = new GraphvizConfigurationOld();
	protected float ranksep = .1f;
	protected float nodesep = .1f;
	protected Object[] modes = { SELECTED_ONLY, SELECTED_TO_ROOT,MINIMAL_SELECTION_GRAPH };
	protected JComboBox modeList = new JComboBox(modes);
	protected Object[] shapeArr = { "box", "ellipse", "egg", "triangle",
			"diamond", "parallelogram", "house", "pentagon", "hexagon",
			"septagon", "octagon", "invtriangle" };
	protected JComboBox nodeShapeList = new JComboBox(shapeArr);
	protected JComboBox typeShapeList = new JComboBox(shapeArr);
	protected JComboBox obsoleteShapeList = new JComboBox(shapeArr);
	protected JCheckBox flipoverBox = new JCheckBox("Draw graph with root on top");
	protected JCheckBox showIDsBox = new JCheckBox("Show ids");
	protected JTabbedPane optionsPane = new JTabbedPane();
	protected LinkDatabase linkDatabase;
	protected SelectionListener selectionListener = new SelectionListener() {

		public void selectionChanged(SelectionEvent e) {
			update();
			reloadImage();
			// logger.info("now updating selection.");
		}
	};
	JComboBox formatBox = new JComboBox(formatList);
	LinkListener linkListener = new LinkListener(this);
	JPanel imagePanel = new JPanel();
	JLabel imageLabel = new JLabel();
	JEditorPane htmlPane = new JEditorPane();
	JScrollPane pane = new JScrollPane(htmlPane,
			ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	JButton optionButton = new JButton("Options");
	JButton saveButton = new JButton("Save");

	public GraphvizCanvas(String id) {
		super(id);
		formatBox.setBackground(Preferences.defaultButtonColor());
		imagePanel.setLayout(new BorderLayout());
		imagePanel.add(imageLabel, "Center");
		setLayout(new BorderLayout());
		// bip.setLayout(new BorderLayout());
		htmlPane.setContentType("text/html");
		htmlPane.setEditable(false);
		htmlPane.addHyperlinkListener(linkListener);
		optionButton.setBackground(Preferences.defaultButtonColor());
		saveButton.setBackground(Preferences.defaultButtonColor());
		Box buttonBox = new Box(BoxLayout.X_AXIS);
		buttonBox.add(Box.createHorizontalGlue());
		buttonBox.add(optionButton);
		buttonBox.add(Box.createHorizontalStrut(10));
		buttonBox.add(modeList);
		buttonBox.add(Box.createHorizontalStrut(10));
		buttonBox.add(primaryFiltersCheckbox);
		buttonBox.add(Box.createHorizontalStrut(10));
		buttonBox.add(saveButton);
		buttonBox.add(Box.createHorizontalGlue());
		add(buttonBox, "South");
		add(pane, "Center");

		primaryFiltersCheckbox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setDoFiltering(primaryFiltersCheckbox.isSelected());
			}
		});
		modeList.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mode = (String) modeList.getSelectedItem();
				reloadImage();
			}
		});
		optionButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showOptions();
			}
		});
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				storeImage();
			}
		});
	}

	public void cleanup() {
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);
	}

	@Override
	public ComponentConfiguration getConfiguration() {
		return new GraphvizViewerComponentConfigurationNew();
	}
	
	@Override
	public ConfigurationPanel getConfigurationPanel() {
		return new GraphvizConfigurationPanelNew(this);
	}
	
	

	public void init() {
		linkDatabase = SessionManager.getManager().getSession().getLinkDatabase();
		setPreferredSize(new Dimension(200, 200));
		formatBox.setFont(getFont());
		modeList.setFont(getFont());
		imageLabel.setFont(getFont());
		optionButton.setFont(getFont());
		saveButton.setFont(getFont());
		flipoverBox.setFont(getFont());
		showIDsBox.setFont(getFont());
		primaryFiltersCheckbox.setFont(getFont());
		modeList.setBackground(Preferences.defaultButtonColor());
		SelectionManager.getManager().addSelectionListener(selectionListener);
		modeList.setSelectedItem(mode);
		reloadImage();
	}

	public void setConfiguration(ComponentConfiguration config) {
	
		if (graphvizConfigurationOldInstance instanceof GraphvizConfigurationOld
				&& graphvizConfigurationOldInstance != null) {
			this.graphvizConfigurationOldInstance = (GraphvizConfigurationOld) graphvizConfigurationOldInstance;
		}
		setDoFiltering(this.graphvizConfigurationOldInstance.getDoFiltering());
		reloadImage();

	}

	public void update() {
		if (SelectionManager.getGlobalSelection().isEmpty()) {
			validate();
			repaint();
		} else {
			validate();
			repaint();
			// doUpdate();
		}
	}

	protected String convertID(String id) {
		return id.replace(':', '_');
	}

	protected String formatLabel(String name) {
		StringBuffer out = new StringBuffer();
		String spacerTokens = "-_, \t";
		StringTokenizer tokenizer = new StringTokenizer(name, spacerTokens,
				true);
		int linelength = 0;
		boolean first = true;
		while (tokenizer.hasMoreElements()) {
			String str = (String) tokenizer.nextElement();
			linelength += str.length();
			out.append(str);
			if (!first && spacerTokens.indexOf(str) != -1) {
				if (linelength + str.length() > MAX_LINE_LENGTH) {
					out.append("\\n");
					linelength = 0;
				}
			}
			first = false;
		}
		return out.toString();
	}

	protected ColorPair getColor(Link tr) {
		ColorPair c = (ColorPair) graphvizConfigurationOldInstance.getColorMap().get(
				tr.getType().getID());
		if (c == null)
			c = defaultLabelColors;
		// if (TermUtil.isRedundant(tr)) {
		// c = defaultRedundantColors;
		// }
		return c;
	}

	protected Color getColor(String s) {
		if (s == null)
			return null;
		try {
			return new Color(Integer.parseInt(s));
		} catch (NumberFormatException ex) {
		}
		return null;
	}

	protected String getColorString(Color color) {
		float[] floats = Color.RGBtoHSB(color.getRed(), color.getGreen(), color
				.getBlue(), null);
		return floats[0] + " " + floats[1] + " " + floats[2];
	}

	protected String getOptions(Link tr) {
		return "label=\"" + formatLabel(tr.getType().getName()) + "\", "
				+ "dir=" + (graphvizConfigurationOldInstance.getFlipOver() ? "back" : "forward")
				+ ", " + (TermUtil.isImplied(tr) ? "style=dotted," : "")
				+ "fontsize=" + graphvizConfigurationOldInstance.getLabelFont().getSize()
				+ ", fontname=\"" + graphvizConfigurationOldInstance.getLabelFont().getFontName()
				+ "\", " + "color=\"" + getColorString(getColor(tr).edge)
				+ "\", " + "fontcolor=\"" + getColorString(getColor(tr).label)
				+ "\"";
	}

	protected String getOptions(OBOClass t) {
		Color fontColor = graphvizConfigurationOldInstance.getTermFontColor();
		if (t.isObsolete())
			fontColor = graphvizConfigurationOldInstance.getObsoleteFontColor();
		else if (TermUtil.isProperty(t))
			fontColor = graphvizConfigurationOldInstance.getTypeFontColor();
		Color strokeColor = graphvizConfigurationOldInstance.getTermStrokeColor();
		if (t.isObsolete())
			strokeColor = graphvizConfigurationOldInstance.getObsoleteStrokeColor();
		else if (TermUtil.isProperty(t))
			strokeColor = graphvizConfigurationOldInstance.getTypeStrokeColor();
		String s = "label=\"" + formatLabel(t.getName())
				+ (graphvizConfigurationOldInstance.getShowIDs() ? "\\n\\n" + t.getID() : "")
				+ "\", " + "shape=" + getShape(t) + ", fontsize="
				+ graphvizConfigurationOldInstance.getNodeFont().getSize() + ", " + "fontname=\""
				+ graphvizConfigurationOldInstance.getNodeFont().getFontName() + "\", "
				+ "fillcolor=\""
				+ getColorString(graphvizConfigurationOldInstance.getTermBoxColor()) + "\""
				+ ", color=\"" + getColorString(strokeColor) + "\""
				+ ", fontcolor=\"" + getColorString(fontColor) + "\", "
				+ "style=filled, URL=\"file:" + t.getID() + "\"";
		return s;
	}

	protected String getShape(OBOClass t) {
		if (t.isObsolete())
			return graphvizConfigurationOldInstance.getObsoleteShape();
		else if (TermUtil.isProperty(t))
			return graphvizConfigurationOldInstance.getTypeShape();
		else
			return graphvizConfigurationOldInstance.getNodeShape();
	}

	protected void outputFile(File textFile) throws IOException {
		PrintWriter writer = new PrintWriter(new FileOutputStream(textFile));
		writer.println("digraph G {");
		writer.println("bgcolor=\""
				+ getColorString(graphvizConfigurationOldInstance.getBGColor()) + "\";");
		writer.println("ranksep=\"" + ranksep + "\";");
		writer.println("nodesep=\"" + nodesep + "\";");
		HashSet relationshipSet = new HashSet();
		populateSet(relationshipSet);
		// logger.info("DEBUG : GraphPlugin : outputFile : relationSet size = "
		// + relationshipSet.size());
		Iterator it = relationshipSet.iterator();
		Set termSet = new HashSet();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			if (tr.getParent() != null) {
				if (graphvizConfigurationOldInstance.getFlipOver()) {
					writer.println(convertID(tr.getParent().getID()) + " -> "
							+ convertID(tr.getChild().getID()) + " ["
							+ getOptions(tr) + "];");
				} else {
					writer.println(convertID(tr.getChild().getID()) + " -> "
							+ convertID(tr.getParent().getID()) + " ["
							+ getOptions(tr) + "];");
				}
				termSet.add(tr.getParent());
			}
			termSet.add(tr.getChild());
		}

		it = termSet.iterator();
		while (it.hasNext()) {
			OBOClass t = (OBOClass) it.next();
			writer.println(convertID(t.getID()) + " [" + getOptions(t) + "];");
		}
		writer.println("}");
		writer.close();
	}

	protected void populateMinimalSelectionGraph(Set set) {
		populateSelectedToRoot(set);
		trimSet(set);
	}

	protected void populateSelectedOnly(Set set) {
		set.addAll(SelectionManager.getManager().getSelection().getTerms());
		// This is not working because the getTerms class was changed in the
		// move to oboedit 2 and this needs to accommodate this
		// in the same way that populateSelectedToRoot below accommodates it.
		// getTerms returns a set of terms of type linked object but set.addAll
		// wants links.

	}

	protected void populateSelectedToRoot(Set set) {
		Iterator it = SelectionManager.getManager().getSelection().getTerms()
				.iterator();
		HashSet lookedAt = new HashSet();
		while (it.hasNext()) {
			Object o = it.next();
			if ((o instanceof Link)) {
				Link tr = (Link) o;
				if (TermUtil.containsLink(linkDatabase, tr))
					set.add(tr);
				LinkedObject t = tr.getChild();
				populateSelectedToRoot(set, t, lookedAt);

				// the two lines below were added to make oboedit 2 work.
				// they change it to expect terms instead of links.
			} else if (o instanceof LinkedObject) {
				populateSelectedToRoot(set, (LinkedObject) o, lookedAt);
			}
		}
	}

	protected void populateSelectedToRoot(Set set, LinkedObject term,
			HashSet lookedAt) {
		if (lookedAt.contains(term))
			return;
		lookedAt.add(term);
		Iterator it = linkDatabase.getParents(term).iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			set.add(tr);
			populateSelectedToRoot(set, tr.getParent(), lookedAt);
		}
	}

	protected void populateSet(Set relationshipSet) {
		if (mode.equals(SELECTED_ONLY)) {
			populateSelectedOnly(relationshipSet);
		} else if (mode.equals(SELECTED_TO_ROOT)) {
			populateSelectedToRoot(relationshipSet);
		} else if (mode.equals(MINIMAL_SELECTION_GRAPH)) {
			populateMinimalSelectionGraph(relationshipSet);
		}
	}

	protected void reloadImage() {
		if (1 == 1 || SelectionManager.getGlobalSelection().isEmpty())
			try {
				File imageFile = File.createTempFile("graphimage", "."
						+ graphvizConfigurationOldInstance.getViewerFormat());
				File textFile = File.createTempFile("graphtext", ".txt");

				File noDisjointTextFile = File.createTempFile("graphtextNoDisjoint", ".txt");
				outputFile(textFile);
				
				
				//This is the section to remove disjoints from the text file.
				//It would be better as a separate method but I cannot get
				//this method to access the newly generated text file if
				// it is made in the other method. The method skeleton and method call are 
				//left intact.
				removeDisjoints(textFile, noDisjointTextFile);
				
				
							Process p = Runtime.getRuntime().exec(
				graphvizConfigurationOldInstance.getDotPath() + " -T"
				+ graphvizConfigurationOldInstance.getViewerFormat() + " -o "
				+ imageFile.getPath() + " " + noDisjointTextFile.getPath());
	
				// logger.info(configuration.getDotPath() + " -T"
				// + configuration.getViewerFormat() + " -o "
				// + imageFile.getPath() + " " + textFile.getPath());

				p.waitFor();
				
				p = Runtime.getRuntime().exec(
				graphvizConfigurationOldInstance.getDotPath() + " -Tcmapx "
				+ noDisjointTextFile.getPath());

				// logger.info(configuration.getDotPath() + " -Tcmapx "
				// + textFile.getPath());
				StringBuffer buffer = new StringBuffer();
				buffer.append("<html>\n");
				buffer.append("<head>\n");

				BufferedReader reader = new BufferedReader(
						new InputStreamReader(p.getInputStream()));
				String line;
				while ((line = reader.readLine()) != null) {
					if (line.endsWith(" />"))
						line = line.substring(0, line.length() - 3) + ">";
					buffer.append(line + "\n");
				}
				p.waitFor();
				htmlPane.setBackground(graphvizConfigurationOldInstance.getBGColor());
				buffer.append("</head>\n");
				buffer.append("<body>\n");
				buffer.append("<img src='file:" + imageFile
						+ "' usemap='#G'>\n");
				buffer.append("</body>\n");
				buffer.append("</html>\n");
				imageLabel.setText(null);
				imageLabel.setIcon(new ImageIcon(imageFile.getPath()));
				// imagePanel.setBackground(bgcolor);
				// htmlPane.setBackground(bgcolor);
				htmlPane.setText(buffer.toString());

				imageFile.deleteOnExit();
				textFile.deleteOnExit();
			} catch (Exception ex) {
				String failureHTML = "<html><center>Could not load GraphViz package.<br>"
						+ "Make sure your properly obtained and installed GraphViz from<br>"
						+ "<b>http://www.research.att.com/sw/tools/graphviz/download.html</b><br>"
						+ "and be sure that the correct path to the executable file<br>"
						+ "is specified in the options window.</center></html>";
				imageLabel.setIcon(null);
				imageLabel.setText(failureHTML);
				ex.printStackTrace();
			}
	}

	/*
	 * Method to remove lines including the string 'disjoint' from
	 * the text file created by graphviz, before the file is converted to an
	 * image file. 
	 */
	public void removeDisjoints(File textFile, File noDisjointTextFile) {
		try
		{
			BufferedReader textFileBufferedReader = new BufferedReader(new FileReader(textFile));
			String graphvizTextFileLine = new String();
			String text_to_be_deleted="isjoint";
			
			PrintWriter noDisjointPrintWriter = new PrintWriter(new FileOutputStream(noDisjointTextFile));

			while( (graphvizTextFileLine=textFileBufferedReader.readLine())!=null )
			{
				//System.out.println(graphvizTextFileLine);
				if(graphvizTextFileLine.contains(text_to_be_deleted))
				{//This is working. Only disjoint lines print. 
					System.out.println(graphvizTextFileLine); }
				else
				{
					//The file is created but empty so this must be the problem.
					//print statements do not print to the file either, though the file is made.
					noDisjointPrintWriter.println(graphvizTextFileLine);
			
				}
			}
			noDisjointPrintWriter.close();
		}
		catch(Exception e){e.printStackTrace();}
	}

	protected void setDoFiltering(boolean doFiltering) {
		graphvizConfigurationOldInstance.setDoFiltering(doFiltering);
		primaryFiltersCheckbox.setSelected(doFiltering);
		if (!doFiltering)
			linkDatabase = SessionManager.getManager().getSession()
					.getLinkDatabase();
		else
			linkDatabase = SessionManager.getManager().getSession()
					.getLinkDatabase();
		;
		reloadImage();
	}

	
	//This is the part that adds all the panels to the options window.
	protected void showOptions() {
		JPanel panel = new JPanel();
		panel.setBackground(Preferences.defaultBackgroundColor());
		panel.setLayout(new BorderLayout());
		final FontChooser linkFontChooser = new FontChooser();
		final FontChooser nodeFontChooser = new FontChooser();
		final JButton commitButton = new JButton("Save Options");
		final JTextField appPathField = new JTextField(graphvizConfigurationOldInstance.getDotPath());
		commitButton.setBackground(Preferences.defaultButtonColor());
		commitButton.setFont(getFont());
		linkFontChooser.setFont(getFont());
		nodeFontChooser.setFont(getFont());
		linkFontChooser.setChosenFont(graphvizConfigurationOldInstance.getLabelFont());
		nodeFontChooser.setChosenFont(graphvizConfigurationOldInstance.getNodeFont());
		nodeShapeList.setSelectedItem(graphvizConfigurationOldInstance.getNodeShape());
		typeShapeList.setSelectedItem(graphvizConfigurationOldInstance.getTypeShape());
		obsoleteShapeList.setSelectedItem(graphvizConfigurationOldInstance.getObsoleteShape());
		flipoverBox.setSelected(graphvizConfigurationOldInstance.getFlipOver());
		showIDsBox.setSelected(graphvizConfigurationOldInstance.getShowIDs());
		formatBox.setSelectedItem(graphvizConfigurationOldInstance.getViewerFormat());
		TitledBorder linkFontBorder = new TitledBorder("Relationship type font");
		TitledBorder nodeFontBorder = new TitledBorder("Term name font");
		linkFontChooser.setBorder(linkFontBorder);
		nodeFontChooser.setBorder(nodeFontBorder);
		linkFontChooser.setOpaque(false);
		nodeFontChooser.setOpaque(false);
		JLabel noTypeLabel = new JLabel("no type selected");
		Vector data = graphvizConfigurationOldInstance.getNamedColorList();

		final ListEditor typeColorList = new ListEditor(new ColorEditor(),
				noTypeLabel, new Vector(), true, true, false, true, false);
		typeColorList.setData(data);
		typeColorList.setVectorEditable(false);

		JPanel fontPanel = new JPanel();
		fontPanel.setOpaque(false);
		fontPanel.setLayout(new BoxLayout(fontPanel, BoxLayout.Y_AXIS));
		fontPanel.add(linkFontChooser);
		fontPanel.add(nodeFontChooser);

		JPanel outerFontPanel = new JPanel();
		outerFontPanel.setBackground(Preferences.defaultBackgroundColor());
		outerFontPanel.setLayout(new BorderLayout());
		outerFontPanel.add(fontPanel, "North");
	
		JPanel shapePanel = new JPanel();
		shapePanel.setBackground(Preferences.defaultBackgroundColor());
		shapePanel.setLayout(new BoxLayout(shapePanel, BoxLayout.Y_AXIS));
		TitledBorder nodeBorder = new TitledBorder("Term shape");
		TitledBorder obsoleteBorder = new TitledBorder("Obsolete shape");
		TitledBorder typeBorder = new TitledBorder("Type shape");
		nodeBorder.setTitleFont(getFont());
		obsoleteBorder.setTitleFont(getFont());
		typeBorder.setTitleFont(getFont());

		JPanel nodeShapePanel = new JPanel();
		nodeShapePanel.setOpaque(false);
		nodeShapePanel.setLayout(new BorderLayout());
		nodeShapePanel.add(nodeShapeList, "Center");
		nodeShapeList.setBackground(Preferences.defaultButtonColor());
		nodeShapePanel.setBorder(nodeBorder);

		JPanel obsoleteShapePanel = new JPanel();
		obsoleteShapePanel.setOpaque(false);
		obsoleteShapePanel.setLayout(new BorderLayout());
		obsoleteShapePanel.add(obsoleteShapeList, "Center");
		obsoleteShapeList.setBackground(Preferences.defaultButtonColor());
		obsoleteShapePanel.setBorder(obsoleteBorder);
		// obsoleteShapeList.setBorder(obsoleteBorder);

		JPanel typeShapePanel = new JPanel();
		typeShapePanel.setOpaque(false);
		typeShapePanel.setLayout(new BorderLayout());
		typeShapePanel.add(typeShapeList, "Center");
		typeShapeList.setBackground(Preferences.defaultButtonColor());
		typeShapePanel.setBorder(typeBorder);
		// typeShapeList.setBorder(typeBorder);

		nodeShapeList.setFont(getFont());
		typeShapeList.setFont(getFont());
		obsoleteShapeList.setFont(getFont());

		shapePanel.add(nodeShapePanel);
		shapePanel.add(Box.createVerticalStrut(5));
		shapePanel.add(obsoleteShapePanel);
		shapePanel.add(Box.createVerticalStrut(5));
		shapePanel.add(typeShapePanel);

		JTextArea messageArea = new JTextArea(
				"This should contain the path to the \"dot\" or "
						+ "\"dot.exe\" file included with the GraphViz "
						+ "software package. The package can be obtained "
						+ "from " + "http://www.research.att.com/sw/"
						+ "tools/graphviz/download.html", 3, 20);
		messageArea.setEditable(false);
		messageArea.setBorder(null);
		messageArea.setOpaque(false);
		messageArea.setLineWrap(true);
		messageArea.setWrapStyleWord(true);

		JButton browseButton = new JButton("Browse...");
		browseButton.setFont(Preferences.getPreferences().getFont());
		browseButton.setBackground(Preferences.defaultButtonColor());
		browseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser chooser = new JFileChooser();
				if (chooser.showOpenDialog(GraphvizCanvas.this) == JFileChooser.APPROVE_OPTION) {
					File file = chooser.getSelectedFile();
					File macPath = new File(file, "Contents/MacOS/dot");
					if (!file.getName().equals("dot") && file.isDirectory()
							&& macPath.exists()) {
						appPathField.setText(macPath.toString());
					} else
						appPathField.setText(file.toString());
				}
			}
		});

		JPanel outerShapePanel = new JPanel();
		outerShapePanel.setBackground(Preferences.defaultBackgroundColor());
		outerShapePanel.setLayout(new BorderLayout());
		outerShapePanel.add(shapePanel, "North");

		JLabel appLabel = new JLabel("Application path");
		appLabel.setFont(getFont());

		Box horzBox = new Box(BoxLayout.X_AXIS);
		horzBox.add(Box.createHorizontalStrut(5));
		horzBox.add(appPathField);
		horzBox.add(Box.createHorizontalStrut(5));

		JPanel appLine = new JPanel();
		appLine.setLayout(new BorderLayout());
		appLine.add(appLabel, "West");
		appLine.add(horzBox, "Center");
		appLine.add(browseButton, "East");
		appLine.setOpaque(false);

		flipoverBox.setOpaque(false);
		showIDsBox.setOpaque(false);

		JLabel formatLabel = new JLabel("Default display format");
		formatLabel.setFont(getFont());

		JPanel viewerFormatLine = new JPanel();
		viewerFormatLine.setOpaque(false);
		viewerFormatLine.setLayout(new BorderLayout());
		viewerFormatLine.add(formatLabel, "West");
		viewerFormatLine.add(Box.createHorizontalStrut(5));
		viewerFormatLine.add(formatBox);

		JPanel appPanel = new JPanel();
		appPanel.setBackground(Preferences.defaultBackgroundColor());
		appPanel.setLayout(new BoxLayout(appPanel, BoxLayout.Y_AXIS));
		// appPanel.add(appLabel);
		// appPanel.add(appPathField);
		appPanel.add(Box.createVerticalStrut(10));
		appPanel.add(viewerFormatLine);
		appPanel.add(Box.createVerticalStrut(10));
		appPanel.add(flipoverBox);
		appPanel.add(Box.createVerticalStrut(10));
		appPanel.add(showIDsBox);
		appPanel.add(Box.createVerticalStrut(10));
		appPanel.add(appLine);
		appPanel.add(Box.createVerticalStrut(10));
		appPanel.add(messageArea);

		JPanel outerAppPanel = new JPanel();
		outerAppPanel.setBackground(Preferences.defaultBackgroundColor());
		outerAppPanel.setLayout(new BorderLayout());
		outerAppPanel.add(appPanel, "North");

		optionsPane.removeAll();
		optionsPane.addTab("Fonts", outerFontPanel);
		optionsPane.addTab("Colors", typeColorList);
		optionsPane.addTab("Shapes", outerShapePanel);
		optionsPane.addTab("Behavior", outerAppPanel);

		panel.add(optionsPane, "Center");

		panel.add(commitButton, "South");

		final JDialog pane = new JDialog((Frame) null, true);
		pane.setContentPane(panel);
		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pane.setVisible(false);
				graphvizConfigurationOldInstance.setDotPath(appPathField.getText());
				graphvizConfigurationOldInstance.setLabelFont(linkFontChooser.getChosenFont());
				graphvizConfigurationOldInstance.setNodeFont(nodeFontChooser.getChosenFont());

				graphvizConfigurationOldInstance.setTypeShape((String) typeShapeList
						.getSelectedItem());
				graphvizConfigurationOldInstance.setNodeShape((String) nodeShapeList
						.getSelectedItem());
				graphvizConfigurationOldInstance.setObsoleteShape((String) obsoleteShapeList
						.getSelectedItem());

				graphvizConfigurationOldInstance.setFlipOver(flipoverBox.isSelected());
				graphvizConfigurationOldInstance.setShowIDs(showIDsBox.isSelected());

				for (int i = 0; i < typeColorList.getData().size(); i++) {
					Object o = typeColorList.getData().get(i);
					if (o instanceof TypeColorPair) {
						TypeColorPair tc = (TypeColorPair) o;
						graphvizConfigurationOldInstance.getColorMap().put(tc.getTypeID(),
								tc.getPair());
					} else if (o instanceof NamedColor) {
						NamedColor nc = (NamedColor) o;
						graphvizConfigurationOldInstance
								.setNamedColor(nc.getName(), nc.getColor());
					}
				}

				graphvizConfigurationOldInstance.setViewerFormat((String) formatBox
						.getSelectedItem());

				pane.dispose();
				reloadImage();
			}
		});
		pane.pack();
		pane.setSize(600, 400);
		pane.show();
	}

	protected void storeImage() {
		try {
			JFileChooser chooser = new JFileChooser();
			chooser.addChoosableFileFilter(new ExtensionFilter(".jpg",
					".jpg - JPEG Format"));
			chooser.addChoosableFileFilter(new ExtensionFilter(".png",
					".png - PNG Format"));
			chooser.addChoosableFileFilter(new ExtensionFilter(".ps", ".ps - "
					+ "Postscript Format"));
			chooser.addChoosableFileFilter(new ExtensionFilter(".dot",
					".dot - GraphViz DOT Format"));
			chooser.addChoosableFileFilter(new ExtensionFilter(".xdot",
					".xdot - GraphViz extended DOT format"));
			chooser.addChoosableFileFilter(new ExtensionFilter(".gif",
					".gif - GIF Format"));

			// Shows the dialog to the user and wait for is answer
			int userChoice = chooser.showSaveDialog(this);

			// check the user answer, is he press "ok" continue in the if block
			if (userChoice == JFileChooser.APPROVE_OPTION) {
				File textFile = File.createTempFile("graphtext", ".txt");
				// logger.info("DEBUG : GraphPlugin : storeImage : temp file
				// name = " + textFile.getAbsolutePath());

				// Creating the .dot file for graphviz
				outputFile(textFile);

				// Getting the selected file extension
				ExtensionFilter ef = (ExtensionFilter) chooser.getFileFilter();

				// Getting the file name (with the full path)
				String outputFile = chooser.getSelectedFile().getPath();

				// Adding the file extension if missing
				if (!outputFile.endsWith(ef.getExt())) {
					outputFile += ef.getExt();
				}

				// logger.info(configuration.getDotPath() + " -T"
				// + ef.getExtNoDot() + " -o " + outputFile + " -v "
				// + textFile.getPath());
				Process p = Runtime.getRuntime().exec(
						graphvizConfigurationOldInstance.getDotPath() + " -T" + ef.getExtNoDot()
								+ " -o  " + outputFile + "  -v "
								+ textFile.getPath());
				/*
				 * 		 The bug has been fixed by putting escaped quotes round the
				 *	 output file
				 *	 string because the 'Documents and Settings' path in
				 *	 the save instructions was being confused by the spaces and
				 *	 was saving to path/Documents which doesn't exist.
				 *	 C:\Program Files\ATT\Graphviz\bin\dot.exe -Tjpg -o
				 *	 C:\Documents and Settings\Jennifer Clark\Desktop\fish.jpg -v
				 *	 C:\DOCUME~1\JENNIF~1\LOCALS~1\Temp\graphtext12278.txt
				 *	 This only matters because the command is run on the cmd line.
				 *
				 */
				p.waitFor();

				textFile.delete();
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	protected void trimSet(Set set) {
		HashSet roots = new HashSet();
		HashSet trash = new HashSet();
		HashSet inUse = new HashSet();
		do {
			Iterator it = set.iterator();
			roots.clear();
			trash.clear();
			while (it.hasNext()) {
				Link tr = (Link) it.next();
				Iterator it2 = set.iterator();
				boolean found = false;
				while (it2.hasNext()) {
					Link tr2 = (Link) it2.next();
					if (tr.getParent() == null)
						continue;
					if (tr.getParent().equals(tr2.getChild())) {
						found = true;
						break;
					}
				}
				if (!found && tr.getParent() != null) {
					roots.add(tr.getParent());
				}
			}
			it = roots.iterator();
			while (it.hasNext()) {
				OBOClass root = (OBOClass) it.next();

				Iterator it2 = set.iterator();
				inUse.clear();
				while (it2.hasNext()) {
					Link tr2 = (Link) it2.next();
					if (tr2.getParent() == null)
						continue;
					if (tr2.getParent().equals(root)
							&& !SelectionManager.getManager().getSelection()
									.getTerms().contains(tr2)) {
						inUse.add(tr2);
					}
				}
				if (inUse.size() < 2) {
					trash.addAll(inUse);
				}
			}
			set.removeAll(trash);
		} while (trash.size() > 0);
	}

}
