package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.swing.GenericEditorComponent;
import org.bbop.swing.ListEditor;
import org.bbop.swing.widget.FontChooser;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;

import org.apache.log4j.*;

public class GraphvizCanvas extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphvizCanvas.class);


	private class ColorEditor extends JPanel implements GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		JColorChooser textColorChooser = new JColorChooser();
		JColorChooser lineColorChooser = new JColorChooser();

		JTabbedPane tabbedPane = new JTabbedPane();

		protected ListEditor editor;

		protected ChangeListener cl = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				editor.commit();
			}
		};

		public ColorEditor() {
			lineColorChooser.setFont(getFont());
			textColorChooser.setFont(getFont());

			setLayout(new BoxLayout(ColorEditor.this, BoxLayout.Y_AXIS));

			attachListeners();
		}

		protected void attachListeners() {
			textColorChooser.getSelectionModel().addChangeListener(cl);
			lineColorChooser.getSelectionModel().addChangeListener(cl);
		}

		public Object createNewValue() {
			return null;
		}

		public void load(Object o) {
			removeAll();
			removeListeners();
			if (o instanceof TypeColorPair) {
				tabbedPane.removeAll();
				tabbedPane.addTab("Type label color", textColorChooser);
				tabbedPane.addTab("Type line color", lineColorChooser);
				TypeColorPair cp = (TypeColorPair) o;
				textColorChooser.setColor(cp.getPair().label);
				lineColorChooser.setColor(cp.getPair().edge);
				add(tabbedPane);
			} else if (o instanceof NamedColor) {
				tabbedPane.removeAll();
				NamedColor nc = (NamedColor) o;
				textColorChooser.setColor(nc.getColor());
				add(textColorChooser);
			}
			attachListeners();
		}

		protected void removeListeners() {
			textColorChooser.getSelectionModel().removeChangeListener(cl);
			lineColorChooser.getSelectionModel().removeChangeListener(cl);
		}

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}
		


		public void store(Object o) {
			if (o instanceof TypeColorPair) {
				TypeColorPair cp = (TypeColorPair) o;
				cp.getPair().label = textColorChooser.getColor();
				cp.getPair().edge = lineColorChooser.getColor();
			} else if (o instanceof NamedColor) {
				NamedColor nc = (NamedColor) o;
				nc.color = textColorChooser.getColor();
			}
		}
	}
	public static class ColorPair implements Cloneable, Serializable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		Color edge;
		Color label;
		int id = idgen++;

		public ColorPair() {
		}

		public ColorPair(Color edge, Color label) {
			this.edge = edge;
			this.label = label;
		}

		public Object clone() {
			try {
				ColorPair out = (ColorPair) super.clone();
				out.id = idgen++;
				return out;
			} catch (Exception ex) {
				return null;
			}
		}

		public Color getEdgeColor() {
			return edge;
		}

		public Color getLabelColor() {
			return label;
		}

		public void setEdgeColor(Color edge) {
			this.edge = edge;
		}

		public void setLabelColor(Color label) {
			this.label = label;
		}

		public String toString() {
			return "edgeColor = " + edge + ", labelColor = " + label;
		}
	}
	protected class ExtensionFilter extends javax.swing.filechooser.FileFilter {
		protected String ext;
		protected String desc;

		public ExtensionFilter(String ext, String desc) {
			this.ext = ext;
			this.desc = desc;
		}

		public boolean accept(File f) {
			return f.getName().endsWith(ext);
		}

		public String getDescription() {
			return desc;
		}

		public String getExt() {
			return ext;
		}

		public String getExtNoDot() {
			return ext.substring(1, ext.length());
		}
	}
	

	
	public static class GraphvizConfiguration implements ComponentConfiguration {
		protected Map colorMap = new HashMap();
		protected Color bgcolor = Color.black;

		protected Color termBoxColor = Color.white;
		protected Color termStrokeColor = Color.black;
		protected Color termFontColor = Color.black;

		protected Color typeBoxColor = Color.white;
		protected Color typeStrokeColor = Color.blue;
		protected Color typeFontColor = Color.blue;

		protected Color obsoleteBoxColor = Color.white;
		protected Color obsoleteStrokeColor = Color.red;
		protected Color obsoleteFontColor = Color.red;

		protected String typeShape = "box";
		protected String nodeShape = "ellipse";
		protected String obsoleteShape = "egg";

		protected boolean flipover = true;
		protected boolean showIDs = false;

		protected Font nodeFont = new Font("Arial", 0, 10);
		protected Font labelFont = new Font("Arial", 0, 18);

		protected String viewerFormat = "jpg";
		protected String dotPath = "/" + "dot";

		protected boolean doFiltering = false;

		public GraphvizConfiguration() {
		}

		public Color getBGColor() {
			return bgcolor;
		}

		public Map getColorMap() {
			return colorMap;
		}

		public boolean getDoFiltering() {
			return doFiltering;
		}



		public String getDotPath() {
			return dotPath;
		}

		public boolean getFlipOver() {
			return flipover;
		}

		public Font getLabelFont() {
			return labelFont;
		}

		public Vector getNamedColorList() {
			Vector data = new Vector();
			data.add(new NamedColor(BACKGROUND_COLOR, bgcolor));

			data.add(new NamedColor(TERM_BACKGROUND_COLOR, termBoxColor));
			data.add(new NamedColor(TERM_TEXT_COLOR, termFontColor));
			data.add(new NamedColor(TERM_STROKE_COLOR, termStrokeColor));

			data.add(new NamedColor(TYPE_BACKGROUND_COLOR, typeBoxColor));
			data.add(new NamedColor(TYPE_STROKE_COLOR, typeStrokeColor));
			data.add(new NamedColor(TYPE_TEXT_COLOR, typeFontColor));

			data.add(new NamedColor(OBSOLETE_BACKGROUND_COLOR,
					obsoleteBoxColor));
			data.add(new NamedColor(OBSOLETE_STROKE_COLOR,
					obsoleteStrokeColor));
			data.add(new NamedColor(OBSOLETE_TEXT_COLOR, obsoleteFontColor));

			Iterator it = TermUtil.getRelationshipTypes(SessionManager.getManager().getSession()).iterator();
			while (it.hasNext()) {
				OBOProperty type = (OBOProperty) it.next();
				ColorPair pair;
				// something wrong?
				pair = (ColorPair) colorMap.get(type.getID());
				if (pair == null)
					pair = (ColorPair) defaultLabelColors.clone();

				TypeColorPair tcp = new TypeColorPair(pair, type.getID());

				data.add(tcp);
			}
			return data;
		}

		public Font getNodeFont() {
			return nodeFont;
		}

		public String getNodeShape() {
			return nodeShape;
		}

		public Color getObsoleteBoxColor() {
			return obsoleteBoxColor;
		}

		public Color getObsoleteFontColor() {
			return obsoleteFontColor;
		}

		public String getObsoleteShape() {
			return obsoleteShape;
		}

		public Color getObsoleteStrokeColor() {
			return obsoleteStrokeColor;
		}

		public boolean getShowIDs() {
			return showIDs;
		}

		public Color getTermBoxColor() {
			return termBoxColor;
		}

		public Color getTermFontColor() {
			return termFontColor;
		}

		public Color getTermStrokeColor() {
			return termStrokeColor;
		}

		public Color getTypeBoxColor() {
			return typeBoxColor;
		}

		public Color getTypeFontColor() {
			return typeFontColor;
		}

		public String getTypeShape() {
			return typeShape;
		}

		public Color getTypeStrokeColor() {
			return typeStrokeColor;
		}

		public String getViewerFormat() {
			return viewerFormat;
		}

		public void setBGColor(Color bgcolor) {
			this.bgcolor = bgcolor;
		}

		public void setColorMap(Map colorMap) {
			this.colorMap = colorMap;
		}

		public void setDoFiltering(boolean doFiltering) {
			this.doFiltering = doFiltering;
		}

		public void setDotPath(String dotPath) {
			this.dotPath = dotPath;
		}

		public void setFlipOver(boolean flipover) {
			this.flipover = flipover;
		}

		public void setLabelFont(Font labelFont) {
			this.labelFont = labelFont;
		}

		public void setNamedColor(String name, Color color) {
			if (name.equals(BACKGROUND_COLOR))
				bgcolor = color;
			else if (name.equals(TERM_BACKGROUND_COLOR))
				termBoxColor = color;
			else if (name.equals(TERM_TEXT_COLOR))
				termFontColor = color;
			else if (name.equals(TERM_STROKE_COLOR))
				termStrokeColor = color;
			else if (name.equals(TYPE_BACKGROUND_COLOR))
				typeBoxColor = color;
			else if (name.equals(TYPE_STROKE_COLOR))
				typeStrokeColor = color;
			else if (name.equals(TYPE_TEXT_COLOR))
				typeFontColor = color;
			else if (name.equals(OBSOLETE_BACKGROUND_COLOR))
				obsoleteBoxColor = color;
			else if (name.equals(OBSOLETE_STROKE_COLOR))
				obsoleteStrokeColor = color;
			else if (name.equals(OBSOLETE_TEXT_COLOR))
				obsoleteFontColor = color;
		}

		public void setNodeFont(Font nodeFont) {
			this.nodeFont = nodeFont;
		}

		public void setNodeShape(String nodeShape) {
			this.nodeShape = nodeShape;
		}

		public void setObsoleteBoxColor(Color obsoleteBoxColor) {
			this.obsoleteBoxColor = obsoleteBoxColor;
		}

		public void setObsoleteFontColor(Color obsoleteFontColor) {
			this.obsoleteFontColor = obsoleteFontColor;
		}

		public void setObsoleteShape(String obsoleteShape) {
			this.obsoleteShape = obsoleteShape;
		}

		public void setObsoleteStrokeColor(Color obsoleteStrokeColor) {
			this.obsoleteStrokeColor = obsoleteStrokeColor;
		}

		public void setShowIDs(boolean showIDs) {
			this.showIDs = showIDs;
		}

		public void setTermBoxColor(Color termBoxColor) {
			this.termBoxColor = termBoxColor;
		}

		public void setTermFontColor(Color termFontColor) {
			this.termFontColor = termFontColor;
		}

		public void setTermStrokeColor(Color termStrokeColor) {
			this.termStrokeColor = termStrokeColor;
		}

		public void setTypeBoxColor(Color typeBoxColor) {
			this.typeBoxColor = typeBoxColor;
		}

		public void setTypeFontColor(Color typeFontColor) {
			this.typeFontColor = typeFontColor;
		}

		public void setTypeShape(String typeShape) {
			this.typeShape = typeShape;
		}

		public void setTypeStrokeColor(Color typeStrokeColor) {
			this.typeStrokeColor = typeStrokeColor;
		}

		public void setViewerFormat(String viewerFormat) {
			this.viewerFormat = viewerFormat;
		}
	}
	protected class LinkListener implements HyperlinkListener {
		public void hyperlinkUpdate(HyperlinkEvent e) {
			if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
				JEditorPane pane = (JEditorPane) e.getSource();
				LinkedObject term = (LinkedObject) SessionManager.getManager().getSession().getObject(e.getURL().getPath());
				Collection<LinkedObject> v = new Vector();
				Iterator<Link> it = term.getParents().iterator();
				while (it.hasNext()) {
					v.add(it.next().getParent());
				}
				v.add(term);
				SelectionManager.getManager().selectTerms(GraphvizCanvas.this, v);
			}
		}
	}
	public static class NamedColor implements Serializable, Cloneable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected String name;
		protected Color color;

		public NamedColor() {
		}

		public NamedColor(String name, Color color) {
			this.name = name;
			this.color = color;
		}

		public Color getColor() {
			return color;
		}

		public String getName() {
			return name;
		}

		public void setColor(Color color) {
			this.color = color;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String toString() {
			return name;
		}
	}
	public static class TypeColorPair implements Serializable, Cloneable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		ColorPair pair;
		String typeid;

		public TypeColorPair() {
		}

		public TypeColorPair(ColorPair pair, String typeid) {
			this.pair = pair;
			this.typeid = typeid;
		}

		public ColorPair getPair() {
			return pair;
		}

		public String getTypeID() {
			return typeid;
		}

		public void setPair(ColorPair pair) {
			this.pair = pair;
		}

		public void setTypeID(String typeid) {
			this.typeid = typeid;
		}

		public String toString() {
			return "Link color: " + typeid + ", pair=" + pair;
		}
	}
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static final String BACKGROUND_COLOR = "Background color";
	protected static final String TERM_BACKGROUND_COLOR = "Term background color";
	protected static final String TERM_TEXT_COLOR = "Term text color";

	protected static final String TERM_STROKE_COLOR = "Term stroke color";
	protected static final String TYPE_STROKE_COLOR = "Type stroke color";
	protected static final String TYPE_BACKGROUND_COLOR = "Type background color";

	protected static final String TYPE_TEXT_COLOR = "Type text color";
	protected static final String OBSOLETE_STROKE_COLOR = "Obsolete stroke color";

	protected static final String OBSOLETE_BACKGROUND_COLOR = "Obsolete background color";

	protected static final String OBSOLETE_TEXT_COLOR = "Obsolete text color";
	protected static final String SELECTED_ONLY = "selected only";

	protected static final String SELECTED_TO_ROOT = "selected to root";
	protected static final String MINIMAL_SELECTION_GRAPH = "minimal "
		+ "connected graph";

	protected static final int MAX_LINE_LENGTH = 25;

	protected static final ColorPair defaultLabelColors = new ColorPair(
			Color.blue, Color.blue);
	protected static final ColorPair defaultRedundantColors = new ColorPair(
			Color.red, Color.red);
	protected static final String[] formatList = { "jpg", "gif" };

	static int idgen = 0;
	//added this because dagview has one:
	protected SessionManager sessionManager = SessionManager.getManager();

	protected GraphvizConfiguration configuration = new GraphvizConfiguration();
	protected float ranksep = .1f;

	protected float nodesep = .1f;

	protected Object[] modes = { SELECTED_ONLY, SELECTED_TO_ROOT,
			MINIMAL_SELECTION_GRAPH };

	protected JComboBox modeList = new JComboBox(modes);

	protected Object[] shapeArr = { "box", "ellipse", "egg", "triangle",
			"diamond", "parallelogram", "house", "pentagon", "hexagon",
			"septagon", "octagon", "invtriangle" };


	protected JComboBox nodeShapeList = new JComboBox(shapeArr);

	protected JComboBox typeShapeList = new JComboBox(shapeArr);

	protected JComboBox obsoleteShapeList = new JComboBox(shapeArr);




	protected JCheckBox flipoverBox = new JCheckBox(
	"Draw graph with root on top");

	protected JCheckBox showIDsBox = new JCheckBox("Show ids");

	JComboBox formatBox = new JComboBox(formatList);

	protected JTabbedPane optionsPane = new JTabbedPane();

	protected LinkDatabase linkDatabase;

	LinkListener linkListener = new LinkListener();

	protected SelectionListener selectionListener = new SelectionListener() {

		public void selectionChanged(SelectionEvent e) {
			update();
			reloadImage();
			//logger.info("now updating selection.");
		}
	};
	JPanel imagePanel = new JPanel();

	JLabel imageLabel = new JLabel();

	JEditorPane htmlPane = new JEditorPane();
	JScrollPane pane = new JScrollPane(htmlPane,
			ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	JButton optionButton = new JButton("Options");

	JButton saveButton = new JButton("Save");
	protected String mode = SELECTED_TO_ROOT;

	public JCheckBox primaryFiltersCheckbox = new JCheckBox("Use primary filters", false);




//	public GraphvizCanvas(String id) {
//	super(id);
//	// TODO Auto-generated constructor stub
//	configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.Y_AXIS));
//	configPanel.add(createPanel(allTypesBox));
//	configPanel.add(createPanel(showAnimations));
//	configPanel.add(createPanel(nonTransitiveBox));
//	configPanel.add(createPanel(showBreakdownBox));
//	configPanel.add(createPanel(succinctCheckbox));
//	Box orientationPanel = Box.createHorizontalBox();
//	orientationPanel.add(new JLabel("Panel orientation"));
//	orientationPanel.add(Box.createHorizontalStrut(10));
//	orientationPanel.add(orientationChooser);
//	orientationPanel.add(Box.createHorizontalGlue());
//	configPanel.add(orientationPanel);
//	configPanel.add(Box.createVerticalGlue());
//	}

//	public GraphPlugin() {

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
		buttonBox.add(primaryFiltersCheckbox );
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
		SelectionManager.getManager().removeSelectionListener(selectionListener);

	}

	protected String convertID(String id) {
		return id.replace(':', '_');
	}







	protected String formatLabel(String name) {
		StringBuffer out = new StringBuffer();
		String spacerTokens = "-_, \t";
		StringTokenizer tokenizer = new StringTokenizer(name, spacerTokens, true);
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
		ColorPair c = (ColorPair) configuration.getColorMap().get(
				tr.getType().getID());
		if (c == null)
			c = defaultLabelColors;
//		if (TermUtil.isRedundant(tr)) {
//		c = defaultRedundantColors;
//		}
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

	public ComponentConfiguration getConfiguration() {
		return configuration;
	}

	public String getName() {
	    return "GraphViz Viewer";  // was "Graph Viewer"
	}

	protected String getOptions(Link tr) {
		return "label=\"" + formatLabel(tr.getType().getName()) + "\", "
		+ "dir="
		+ (configuration.getFlipOver() ? "back" : "forward")
		+ ", " + (TermUtil.isImplied(tr) ? "style=dotted," : "")
		+ "fontsize=" + configuration.getLabelFont().getSize()
		+ ", fontname=\""
		+ configuration.getLabelFont().getFontName() + "\", "
		+ "color=\"" + getColorString(getColor(tr).edge) + "\", "
		+ "fontcolor=\"" + getColorString(getColor(tr).label) + "\"";
	}

	protected String getOptions(OBOClass t) {
		Color fontColor = configuration.getTermFontColor();
		if (t.isObsolete())
			fontColor = configuration.getObsoleteFontColor();
		else if (TermUtil.isProperty(t))
			fontColor = configuration.getTypeFontColor();

		Color strokeColor = configuration.getTermStrokeColor();
		if (t.isObsolete())
			strokeColor = configuration.getObsoleteStrokeColor();
		else if (TermUtil.isProperty(t))
			strokeColor = configuration.getTypeStrokeColor();

		String s = "label=\"" + formatLabel(t.getName())
		+ (configuration.getShowIDs() ? "\\n\\n" + t.getID() : "")
		+ "\", " + "shape=" + getShape(t) + ", fontsize="
		+ configuration.getNodeFont().getSize() + ", "
		+ "fontname=\""
		+ configuration.getNodeFont().getFontName() + "\", "
		+ "fillcolor=\""
		+ getColorString(configuration.getTermBoxColor()) + "\""
		+ ", color=\"" + getColorString(strokeColor) + "\""
		+ ", fontcolor=\"" + getColorString(fontColor) + "\", "
		+ "style=filled, URL=\"file:" + t.getID() + "\"";
		return s;
	}

	protected String getShape(OBOClass t) {
		if (t.isObsolete())
			return configuration.getObsoleteShape();
		else if (TermUtil.isProperty(t))
			return configuration.getTypeShape();
		else
			return configuration.getNodeShape();
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

	protected void outputFile(File textFile) throws IOException {
		PrintWriter writer = new PrintWriter(new FileOutputStream(textFile));
		writer.println("digraph G {");

		writer.println("bgcolor=\""
				+ getColorString(configuration.getBGColor()) + "\";");
		writer.println("ranksep=\"" + ranksep + "\";");
		writer.println("nodesep=\"" + nodesep + "\";");

		HashSet relationshipSet = new HashSet();
		populateSet(relationshipSet);

		//logger.info("DEBUG : GraphPlugin : outputFile : relationSet size = " + relationshipSet.size());

		Iterator it = relationshipSet.iterator();
		Set termSet = new HashSet();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			if (tr.getParent() != null) {
				if (configuration.getFlipOver()) {
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
//This is not working because the getTerms class was changed in the 
		//move to oboedit 2 and this needs to accommodate this
		//in the same way that populateSelectedToRoot below accomodates it. 
		//getTerms returns a set of terms of type linked object but set.addAll wants links.
	
	}

	protected void populateSelectedToRoot(Set set) {
		Iterator it = SelectionManager.getManager().getSelection().getTerms().iterator();
		HashSet lookedAt = new HashSet();
		while (it.hasNext()) {
			Object o = it.next();
			if ((o instanceof Link)) {
				Link tr = (Link) o;
				if (TermUtil.containsLink(linkDatabase, tr))
					set.add(tr);
				LinkedObject t = tr.getChild();
				populateSelectedToRoot(set, t, lookedAt);

			//the two lines below were added to make oboedit 2 work. 
				//they change it to expect terms instead of links. 
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
		if (1==1 || SelectionManager.getGlobalSelection().isEmpty())
			try {
				File imageFile = File.createTempFile("graphimage", "."
						+ configuration.getViewerFormat());
				File textFile = File.createTempFile("graphtext", ".txt");

				outputFile(textFile);
				Process p = Runtime.getRuntime().exec(
						configuration.getDotPath() + " -T"
						+ configuration.getViewerFormat() + " -o "
						+ imageFile.getPath() + " " + textFile.getPath());
				//logger.info(configuration.getDotPath() + " -T"
					//	+ configuration.getViewerFormat() + " -o "
						//+ imageFile.getPath() + " " + textFile.getPath());
				p.waitFor();
				p = Runtime.getRuntime().exec(
						configuration.getDotPath() + " -Tcmapx "
						+ textFile.getPath());
				//logger.info(configuration.getDotPath() + " -Tcmapx "
					//	+ textFile.getPath());
				StringBuffer buffer = new StringBuffer();
				buffer.append("<html>\n");
				buffer.append("<head>\n");

				BufferedReader reader = new BufferedReader(new InputStreamReader(p
						.getInputStream()));
				String line;
				while ((line = reader.readLine()) != null) {
					if (line.endsWith(" />"))
						line = line.substring(0, line.length() - 3) + ">";
					buffer.append(line + "\n");
				}
				p.waitFor();
				htmlPane.setBackground(configuration.getBGColor());
				buffer.append("</head>\n");
				buffer.append("<body>\n");
				buffer.append("<img src='file:" + imageFile + "' usemap='#G'>\n");
				buffer.append("</body>\n");
				buffer.append("</html>\n");
				imageLabel.setText(null);
				imageLabel.setIcon(new ImageIcon(imageFile.getPath()));
//				imagePanel.setBackground(bgcolor);
//				htmlPane.setBackground(bgcolor);
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

	public void setConfiguration(ComponentConfiguration configuration) {

		if (configuration instanceof GraphvizConfiguration
				&& configuration != null) {
			this.configuration = (GraphvizConfiguration) configuration;
		}
		setDoFiltering(this.configuration.getDoFiltering());
		reloadImage();
	}

	protected void setDoFiltering(boolean doFiltering) {
		configuration.setDoFiltering(doFiltering);
		primaryFiltersCheckbox.setSelected(doFiltering);
		if (!doFiltering)
			linkDatabase = SessionManager.getManager().getSession().getLinkDatabase();
		else
			linkDatabase = SessionManager.getManager().getSession().getLinkDatabase();;
			reloadImage();
	}

	protected void showOptions() {
		JPanel panel = new JPanel();
		panel.setBackground(Preferences.defaultBackgroundColor());
		panel.setLayout(new BorderLayout());

		final FontChooser linkFontChooser = new FontChooser();
		final FontChooser nodeFontChooser = new FontChooser();

		final JButton commitButton = new JButton("Save Options");
		final JTextField appPathField = new JTextField(configuration
				.getDotPath());

		commitButton.setBackground(Preferences.defaultButtonColor());
		commitButton.setFont(getFont());

		linkFontChooser.setFont(getFont());
		nodeFontChooser.setFont(getFont());

		linkFontChooser.setChosenFont(configuration.getLabelFont());
		nodeFontChooser.setChosenFont(configuration.getNodeFont());

		nodeShapeList.setSelectedItem(configuration.getNodeShape());
		typeShapeList.setSelectedItem(configuration.getTypeShape());
		obsoleteShapeList
		.setSelectedItem(configuration.getObsoleteShape());

		flipoverBox.setSelected(configuration.getFlipOver());
		showIDsBox.setSelected(configuration.getShowIDs());

		formatBox.setSelectedItem(configuration.getViewerFormat());
		/*
		 * JPanel nodeShapePanel = new JPanel(); nodeShapePanel.add(nodeShape);
		 */
		TitledBorder linkFontBorder = new TitledBorder("Relationship type font");
		TitledBorder nodeFontBorder = new TitledBorder("Term name font");

		linkFontChooser.setBorder(linkFontBorder);
		nodeFontChooser.setBorder(nodeFontBorder);
		linkFontChooser.setOpaque(false);
		nodeFontChooser.setOpaque(false);

		JLabel noTypeLabel = new JLabel("no type selected");

		Vector data = configuration.getNamedColorList();
		/*
		 * Vector data = new Vector(); data.add(new NamedColor(BACKGROUND_COLOR,
		 * bgcolor));
		 * 
		 * data.add(new NamedColor(TERM_BACKGROUND_COLOR, termBoxColor));
		 * data.add(new NamedColor(TERM_TEXT_COLOR, termFontColor));
		 * data.add(new NamedColor(TERM_STROKE_COLOR, termStrokeColor));
		 * 
		 * data.add(new NamedColor(TYPE_BACKGROUND_COLOR, typeBoxColor));
		 * data.add(new NamedColor(TYPE_STROKE_COLOR, typeStrokeColor));
		 * data.add(new NamedColor(TYPE_TEXT_COLOR, typeFontColor));
		 * 
		 * data.add(new NamedColor(OBSOLETE_BACKGROUND_COLOR,
		 * obsoleteBoxColor)); data.add(new NamedColor(OBSOLETE_STROKE_COLOR,
		 * obsoleteStrokeColor)); data.add(new NamedColor(OBSOLETE_TEXT_COLOR,
		 * obsoleteFontColor));
		 * 
		 * Iterator it = controller.getHistory().getRelationshipTypes().
		 * iterator(); while(it.hasNext()) { OBOProperty type = (OBOProperty)
		 * it.next(); ColorPair pair; if (colorMap.containsKey(type)) pair =
		 * (ColorPair) colorMap.get(type); else pair = (ColorPair)
		 * defaultLabelColors.clone();
		 * 
		 * TypeColorPair tcp = new TypeColorPair(pair, type);
		 * 
		 * data.add(tcp); }
		 */
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

		// panel.add(typeColorList);

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
				configuration.setDotPath(appPathField.getText());
				configuration
				.setLabelFont(linkFontChooser.getChosenFont());
				configuration.setNodeFont(nodeFontChooser.getChosenFont());

				configuration.setTypeShape((String) typeShapeList
						.getSelectedItem());
				configuration.setNodeShape((String) nodeShapeList
						.getSelectedItem());
				configuration.setObsoleteShape((String) obsoleteShapeList
						.getSelectedItem());

				configuration.setFlipOver(flipoverBox.isSelected());
				configuration.setShowIDs(showIDsBox.isSelected());

				for (int i = 0; i < typeColorList.getData().size(); i++) {
					Object o = typeColorList.getData().get(i);
					if (o instanceof TypeColorPair) {
						TypeColorPair tc = (TypeColorPair) o;
						configuration.getColorMap().put(tc.getTypeID(),
								tc.getPair());
					} else if (o instanceof NamedColor) {
						NamedColor nc = (NamedColor) o;
						configuration.setNamedColor(nc.getName(), nc
								.getColor());
					}
				}

				configuration.setViewerFormat((String) formatBox
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

			// check the user answer, is  he press  "ok" continue in the if  block
			if (userChoice == JFileChooser.APPROVE_OPTION) {
				File textFile = File.createTempFile("graphtext", ".txt");
				//logger.info("DEBUG : GraphPlugin : storeImage : temp file name = " + textFile.getAbsolutePath());

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

				//logger.info(configuration.getDotPath() + " -T"
				//		+ ef.getExtNoDot() + " -o " + outputFile + " -v "
				//		+ textFile.getPath());
				Process p = Runtime.getRuntime().exec(
						configuration.getDotPath() + " -T"
						+ ef.getExtNoDot() + " -o  " + outputFile + "  -v "
						+ textFile.getPath());
				//The bug has been fixed by putting escaped quotes round the output file
				//string because the 'Documents and Settings' path in 
				// the save instructions was being confused by the spaces and 
				//was saving to path/Documents which doesn't exist.
				//C:\Program Files\ATT\Graphviz\bin\dot.exe -Tjpg -o C:\Documents and Settings\Jennifer Clark\Desktop\fish.jpg -v C:\DOCUME~1\JENNIF~1\LOCALS~1\Temp\graphtext12278.txt
				//This only matters because the command is run on the cmd line.

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
							&& !SelectionManager.getManager().getSelection().getTerms().contains(tr2)) {
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


	public void update() {
		if (SelectionManager.getGlobalSelection().isEmpty()) {
				validate();
				repaint();
		
		} else {
			validate();
			repaint();
			//doUpdate();
		}
	}
}
