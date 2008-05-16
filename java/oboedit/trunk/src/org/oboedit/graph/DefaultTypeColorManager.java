package org.oboedit.graph;

import java.awt.Color;
import java.awt.Font;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.OBOProperty;
import org.oboedit.piccolo.PZNodeCache;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.nodes.PText;

import org.apache.log4j.*;

public class DefaultTypeColorManager implements TypeColorManager, TypeIconManager {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultTypeColorManager.class);
	
	protected Map colorMap = new HashMap();
	protected Map iconMap = new HashMap();
	protected List defaultColors = new LinkedList();

	public static int ICON_HEIGHT = 10;
	public static int ICON_WIDTH = 10;
	
	public DefaultTypeColorManager() {
		colorMap.put(OBOProperty.IS_A, Color.blue);
		defaultColors.add(Color.ORANGE);
		defaultColors.add(Color.GREEN);
		defaultColors.add(new Color(150, 150, 0));
		iconMap.put("OBO_REL:is_a", IconBuilderUtil.createIcon(new Ellipse2D.Double(0,0,30,30), new Color(102, 102, 153), null, null, new Font("Serif", Font.BOLD, 36), "i", Color.white));
		iconMap.put("part_of", IconBuilderUtil.createIcon(new Rectangle2D.Double(0,0,30,30), Color.blue, null, null, new Font("Serif", Font.BOLD, 36), "p", Color.white));
	}
	
	public PNode getIcon(OBOProperty type) {
		PNode icon = (PNode) iconMap.get(type.getID());
		if (icon != null) {
			return (PNode) icon.clone();
		} else
			return IconBuilderUtil.createIcon(new Rectangle2D.Double(0,0,30,30), Color.black, null, null, new Font("Serif", Font.BOLD, 36), type.getID(), Color.white);
	}
	
	public void mapColor(OBOProperty type, Paint paint) {
		colorMap.put(type, paint);
	}
	
	protected static Random random = new Random();
	
	public Color getRandomColor() {
		return new Color(random.nextInt(255), random.nextInt(255), random.nextInt(255));
	}

	public Paint getColor(OBOProperty type) {
		Color out = (Color) colorMap.get(type);
		if (out == null) {
			if (defaultColors.size() > 0)
				out = (Color) defaultColors.remove(0);
			else
				out = getRandomColor();
			colorMap.put(type, out);
		}
		// TODO Auto-generated method stub
		return out;
	}

}
