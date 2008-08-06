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
import org.oboedit.gui.components.graphvizViewer.*;
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
	/**
	 * Holds all the configuration options to call graphviz.
	 * 
	 * There should be only one instance of it !! 
	 */
	GraphvizSettings graphvizSettingsInstance = new GraphvizSettings();

	protected float ranksep = .1f;
	protected float nodesep = .1f;
	protected Object[] modes = { SELECTED_ONLY, SELECTED_TO_ROOT,MINIMAL_SELECTION_GRAPH };
	protected JComboBox modeList = new JComboBox(modes);
	 Object[] shapeArr = { "box", "ellipse", "egg", "triangle",
			"diamond", "parallelogram", "house", "pentagon", "hexagon",
			"septagon", "octagon", "invtriangle" };
	 JComboBox nodeShapeList = new JComboBox(shapeArr);
	 JComboBox typeShapeList = new JComboBox(shapeArr);
	 JComboBox obsoleteShapeList = new JComboBox(shapeArr);
	protected JCheckBox flipoverBox = new JCheckBox("Draw graph with root on top");
	protected JCheckBox showIDsBox = new JCheckBox("Show ids");
	protected JTabbedPane optionsPane = new JTabbedPane();
	protected LinkDatabase linkDatabase;
	protected SelectionListener selectionListener = new SelectionListener() {

		public void selectionChanged(SelectionEvent e) {
			update();
			reloadImage();
			 logger.info("now updating selection.");
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
		saveButton.setBackground(Preferences.defaultButtonColor());
		Box buttonBox = new Box(BoxLayout.X_AXIS);
		buttonBox.add(Box.createHorizontalGlue());
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
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				storeImage();
			}
		});
	}
	

	@Override
	public ComponentConfiguration getConfiguration() {
		System.out.println("GraphvizCanvas: getConfiguration() method : config = " + graphvizSettingsInstance);
		return graphvizSettingsInstance;
		
	}

	@Override
	public ConfigurationPanel getConfigurationPanel() {
		System.out.println("GraphvizCanvas: getConfigurationPanel() method: returned " +
		"GraphvizConfigPanel()");
		return new GraphvizConfigPanel(this);
	}


	public void cleanup() {
		
		SelectionManager.getManager()
		.removeSelectionListener(selectionListener);

		System.out.println("GraphvizCanvas: cleanup() method: " +
		"SelectionManager.getManager()" + 
		".removeSelectionListener run on selectionListener");

	}



	public void init() {
		linkDatabase = SessionManager.getManager().getSession().getLinkDatabase();
		setPreferredSize(new Dimension(200, 200));
		formatBox.setFont(getFont());
		modeList.setFont(getFont());
		imageLabel.setFont(getFont());
		saveButton.setFont(getFont());
		flipoverBox.setFont(getFont());
		showIDsBox.setFont(getFont());
		primaryFiltersCheckbox.setFont(getFont());
		modeList.setBackground(Preferences.defaultButtonColor());
		SelectionManager.getManager().addSelectionListener(selectionListener);
		modeList.setSelectedItem(mode);
		reloadImage();
		System.out.println("GraphvizCanvas: init() method.");
	}

	public void setConfiguration(ComponentConfiguration config) {

		if (config != null && config instanceof GraphvizSettings) {
			this.graphvizSettingsInstance = (GraphvizSettings) config;
		}
		setDoFiltering(this.graphvizSettingsInstance.getDoFiltering());
		reloadImage();
		
		System.out.println("GraphvizCanvas: setConfiguration() method.");
		System.out.println("GraphvizCanvas : setConfiguration() : config = " + graphvizSettingsInstance);

	}

	public void update() {
		System.out.println("GraphvizCanvas: update() method.");

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
		System.out.println("GraphvizCanvas: convertID() method.");

		return id.replace(':', '_');
	
	}

	protected String formatLabel(String name) {
		System.out.println("GraphvizCanvas: formatLabel() method.");

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
		// System.out.println("GraphvizCanvas: ColorPair getColor() method.");

		ColorPair c = (ColorPair) graphvizSettingsInstance.getColorMap().get(
				tr.getType().getID());
		if (c == null)
			c = defaultLabelColors;
		// if (TermUtil.isRedundant(tr)) {
		// c = defaultRedundantColors;
		// }
		return c;
	}

	protected Color getColor(String s) {
		
		// System.out.println("GraphvizCanvas: Color getColor() method.");

		if (s == null)
			return null;
		try {
			return new Color(Integer.parseInt(s));
		} catch (NumberFormatException ex) {
		}
		return null;
	}

	protected String getColorString(Color color) {
		
		System.out.println("GraphvizCanvas: getColorString() method.");

		float[] floats = Color.RGBtoHSB(color.getRed(), color.getGreen(), color
				.getBlue(), null);
		return floats[0] + " " + floats[1] + " " + floats[2];
	}

	protected String getOptions(Link tr) {
		
		System.out.println("GraphvizCanvas: Link getOptions() method.");
		
		return "label=\"" + formatLabel(tr.getType().getName()) + "\", "
		+ "dir=" + (graphvizSettingsInstance.getFlipOver() ? "back" : "forward")
		+ ", " + (TermUtil.isImplied(tr) ? "style=dotted," : "")
		+ "fontsize=" + graphvizSettingsInstance.getLabelFont().getSize()
		+ ", fontname=\"" + graphvizSettingsInstance.getLabelFont().getFontName()
		+ "\", " + "color=\"" + getColorString(getColor(tr).edge)
		+ "\", " + "fontcolor=\"" + getColorString(getColor(tr).label)
		+ "\"";
	}

	protected String getOptions(OBOClass t) {
		
		System.out.println("GraphvizCanvas: OBOClass getOptions() method.");

		
		Color fontColor = graphvizSettingsInstance.getTermFontColor();
		if (t.isObsolete())
			fontColor = graphvizSettingsInstance.getObsoleteFontColor();
		else if (TermUtil.isProperty(t))
			fontColor = graphvizSettingsInstance.getTypeFontColor();
		Color strokeColor = graphvizSettingsInstance.getTermStrokeColor();
		if (t.isObsolete())
			strokeColor = graphvizSettingsInstance.getObsoleteStrokeColor();
		else if (TermUtil.isProperty(t))
			strokeColor = graphvizSettingsInstance.getTypeStrokeColor();
		String s = "label=\"" + formatLabel(t.getName())
		+ (graphvizSettingsInstance.getShowIDs() ? "\\n\\n" + t.getID() : "")
		+ "\", " + "shape=" + getShape(t) + ", fontsize="
		+ graphvizSettingsInstance.getNodeFont().getSize() + ", " + "fontname=\""
		+ graphvizSettingsInstance.getNodeFont().getFontName() + "\", "
		+ "fillcolor=\""
		+ getColorString(graphvizSettingsInstance.getTermBoxColor()) + "\""
		+ ", color=\"" + getColorString(strokeColor) + "\""
		+ ", fontcolor=\"" + getColorString(fontColor) + "\", "
		+ "style=filled, URL=\"file:" + t.getID() + "\"";
		return s;
	}

	protected String getShape(OBOClass t) {
		
		System.out.println("GraphvizCanvas: OBOClass getShape() method.");
		
		if (t.isObsolete())
			return graphvizSettingsInstance.getObsoleteShape();
		else if (TermUtil.isProperty(t))
			return graphvizSettingsInstance.getTypeShape();
		else
			return graphvizSettingsInstance.getNodeShape();
	}

	protected void outputFile(File textFile) throws IOException {
		
		System.out.println("GraphvizCanvas: outputFile() method.");
		
		PrintWriter writer = new PrintWriter(new FileOutputStream(textFile));
		writer.println("digraph G {");
		writer.println("bgcolor=\""
				+ getColorString(graphvizSettingsInstance.getBGColor()) + "\";");
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
				if (graphvizSettingsInstance.getFlipOver()) {
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
		
		System.out.println("GraphvizCanvas: populateMinimalSelectionGraph() method.");

		
		populateSelectedToRoot(set);
		trimSet(set);
	}

	protected void populateSelectedOnly(Set set) {
		
		System.out.println("GraphvizCanvas: populateSelectedOnly() method.");
		
		set.addAll(SelectionManager.getManager().getSelection().getTerms());
		// This is not working because the getTerms class was changed in the
		// move to oboedit 2 and this needs to accommodate this
		// in the same way that populateSelectedToRoot below accommodates it.
		// getTerms returns a set of terms of type linked object but set.addAll
		// wants links.

	}

	protected void populateSelectedToRoot(Set set) {
		
		System.out.println("GraphvizCanvas: populateSelectedToRoot() method.");
		
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
		
		System.out.println("GraphvizCanvas: populateSelectedToRoot() method.");

		
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
		
		System.out.println("GraphvizCanvas: populateSet() method.");

		
		if (mode.equals(SELECTED_ONLY)) {
			populateSelectedOnly(relationshipSet);
		} else if (mode.equals(SELECTED_TO_ROOT)) {
			populateSelectedToRoot(relationshipSet);
		} else if (mode.equals(MINIMAL_SELECTION_GRAPH)) {
			populateMinimalSelectionGraph(relationshipSet);
		}
	}

	protected void reloadImage() {
		
		System.out.println("GraphvizCanvas: reloadImage() method.");

		System.out.println("GraphvizCanvas: reloadImage() : " + graphvizSettingsInstance.getLabelFont());
		
		if (1 == 1 || SelectionManager.getGlobalSelection().isEmpty())
			try {
				File imageFile = File.createTempFile("graphimage", "."
						+ graphvizSettingsInstance.getViewerFormat());
				File textFile = File.createTempFile("graphtext", ".txt");

				File noDisjointTextFile = File.createTempFile("graphtextNoDisjoint", ".txt");
				outputFile(textFile);


				removeDisjoints(textFile, noDisjointTextFile);


				Process p = Runtime.getRuntime().exec(
						graphvizSettingsInstance.getDotPath() + " -T"
						+ graphvizSettingsInstance.getViewerFormat() + " -o "
						+ imageFile.getPath() + " " + noDisjointTextFile.getPath());

				// logger.info(configuration.getDotPath() + " -T"
				// + configuration.getViewerFormat() + " -o "
				// + imageFile.getPath() + " " + textFile.getPath());

				p.waitFor();

				p = Runtime.getRuntime().exec(
						graphvizSettingsInstance.getDotPath() + " -Tcmapx "
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
				htmlPane.setBackground(graphvizSettingsInstance.getBGColor());
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
	
		System.out.println("GraphvizCanvas: removeDisjoints() method.");

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
		
		System.out.println("GraphvizCanvas: setDoFiltering() method.");

		
		graphvizSettingsInstance.setDoFiltering(doFiltering);
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

	protected void storeImage() {
		
		System.out.println("GraphvizCanvas: storeImage() method.");

		
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
						graphvizSettingsInstance.getDotPath() + " -T" + ef.getExtNoDot()
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
		
		System.out.println("GraphvizCanvas: trimSet() method.");

		
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
