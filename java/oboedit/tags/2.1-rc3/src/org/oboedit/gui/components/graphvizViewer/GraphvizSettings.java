package org.oboedit.gui.components.graphvizViewer;

import java.awt.Color;
import java.awt.Font;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.bbop.framework.ComponentConfiguration;
import org.obo.datamodel.OBOProperty;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;

public class GraphvizSettings 
	 implements ComponentConfiguration {
	
		//initialize logger
		protected final static Logger logger = Logger.getLogger(GraphvizSettings.class);
		protected Map<String, ColorPair> colorMap = new HashMap<String, ColorPair>();
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

		
		public GraphvizSettings() {
		}

		public Color getBGColor() {
			return bgcolor;
		}

		public Map<String, ColorPair> getColorMap() {
			//logger.debug("GraphvizSettings: colorMap = " + colorMap);
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

		public void setColorMap(Map<String, ColorPair> colorMap) {
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
			if (name.equals(GraphvizCanvas.BACKGROUND_COLOR))
				bgcolor = color;
			else if (name.equals(GraphvizCanvas.TERM_BACKGROUND_COLOR))
				termBoxColor = color;
			else if (name.equals(GraphvizCanvas.TERM_TEXT_COLOR))
				termFontColor = color;
			else if (name.equals(GraphvizCanvas.TERM_STROKE_COLOR))
				termStrokeColor = color;
			else if (name.equals(GraphvizCanvas.TYPE_BACKGROUND_COLOR))
				typeBoxColor = color;
			else if (name.equals(GraphvizCanvas.TYPE_STROKE_COLOR))
				typeStrokeColor = color;
			else if (name.equals(GraphvizCanvas.TYPE_TEXT_COLOR))
				typeFontColor = color;
			else if (name.equals(GraphvizCanvas.OBSOLETE_BACKGROUND_COLOR))
				obsoleteBoxColor = color;
			else if (name.equals(GraphvizCanvas.OBSOLETE_STROKE_COLOR))
				obsoleteStrokeColor = color;
			else if (name.equals(GraphvizCanvas.OBSOLETE_TEXT_COLOR))
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
		
		public Vector<Object> getNamedColorList() {
			Vector<Object> data = new Vector<Object>();
			data.add(new NamedColor(GraphvizCanvas.BACKGROUND_COLOR, bgcolor));

			data.add(new NamedColor(GraphvizCanvas.TERM_BACKGROUND_COLOR, termBoxColor));
			data.add(new NamedColor(GraphvizCanvas.TERM_TEXT_COLOR, termFontColor));
			data.add(new NamedColor(GraphvizCanvas.TERM_STROKE_COLOR, termStrokeColor));

			data.add(new NamedColor(GraphvizCanvas.TYPE_BACKGROUND_COLOR, typeBoxColor));
			data.add(new NamedColor(GraphvizCanvas.TYPE_STROKE_COLOR, typeStrokeColor));
			data.add(new NamedColor(GraphvizCanvas.TYPE_TEXT_COLOR, typeFontColor));

			data.add(new NamedColor(GraphvizCanvas.OBSOLETE_BACKGROUND_COLOR,
					obsoleteBoxColor));
			data.add(new NamedColor(GraphvizCanvas.OBSOLETE_STROKE_COLOR,
					obsoleteStrokeColor));
			data.add(new NamedColor(GraphvizCanvas.OBSOLETE_TEXT_COLOR, obsoleteFontColor));

			Iterator<OBOProperty> it = TermUtil.getRelationshipTypes(SessionManager.getManager().getSession()).iterator();
			while (it.hasNext()) {
				OBOProperty type = it.next();
				ColorPair pair;
				// something wrong?
				pair = colorMap.get(type.getID());
				if (pair == null)
					pair = (ColorPair) GraphvizCanvas.defaultLabelColors.clone();

				TypeColorPair tcp = new TypeColorPair(pair, type.getID());

				data.add(tcp);
			}
			return data;
			
		}

		
	}

	

