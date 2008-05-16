package org.oboedit.gui.filter;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;

import org.bbop.swing.ColorUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.SearchCriterion;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.ObjectSelector;

import org.apache.log4j.*;

public class HeatmapColor implements ColorProvider {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HeatmapColor.class);

	protected Color minColor;
	protected Color maxColor;
	protected Color noValueColor;
	protected String minValue;
	protected String maxValue;
	protected String criterionID;
	protected boolean doBlend;

	public HeatmapColor() {
	}

	public HeatmapColor(ColorProvider c) {
		this(c.getColor(null, null));
	}

	public HeatmapColor(Color color) {
		this.minColor = color;
		this.maxColor = color;
		this.noValueColor = color;
	}

	public Color getColor(ObjectSelector selector, Object o) {
		double min = getValue(selector, minValue, criterionID, 0,
				SessionManager.getManager().getSession());
		double max = getValue(selector, maxValue, criterionID,
				Double.MAX_VALUE, SessionManager.getManager().getSession());
		double val = getExpressionValue(criterionID, Double.NaN, o);
		if (val == Double.NaN)
			return noValueColor;
		double ratio = (val - min) / (max - min);
		if (ratio < 0)
			ratio = 0;
		else if (ratio > 1)
			ratio = 1;
		return getBlendAtRatio(ratio);
	}

	protected Color getBlendAtRatio(double ratio) {
		int red = getColorCompAtRatio(minColor.getRed(), maxColor.getRed(),
				ratio);
		int blue = getColorCompAtRatio(minColor.getBlue(), maxColor.getBlue(),
				ratio);
		int green = getColorCompAtRatio(minColor.getGreen(), maxColor
				.getGreen(), ratio);
		int alpha = getColorCompAtRatio(minColor.getAlpha(), maxColor
				.getAlpha(), ratio);
		return new Color(red, green, blue, alpha);
	}

	protected static int getColorCompAtRatio(int a, int b, double ratio) {
		return (int) (a + ratio * (b - a));
	}

	protected static double getExpressionValue(String exp, double defaultVal,
			Object o) {
		try {
			SearchCriterion crit = FilterManager.getManager().getCriterion(exp);
			if (crit != null) {
				Collection val = crit.getValues(new ArrayList(), o);
				if (val.size() > 0) {
					Object v = val.iterator().next();
					if (v instanceof Number) {
						return ((Number) v).doubleValue();
					}
				}
			}
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return defaultVal;
	}

	protected static double getValue(ObjectSelector selector, String valExp,
			String criterionID, double defaultVal, Object o) {
		if (valExp == null)
			return defaultVal;
		if (valExp.startsWith("$") && valExp.endsWith("$")) {
			String varName = valExp.substring(1, valExp.length() - 1);
			if (varName.equals("min")) {
				return getMinVal(selector, criterionID, defaultVal);
			} else if (varName.equals("max")) {
				return getMaxVal(selector, criterionID, defaultVal);
			} else
				return getExpressionValue(varName, defaultVal, o);
		} else {
			return Double.parseDouble(valExp);
		}
	}

	protected static double getMinVal(ObjectSelector selector,
			String criterionID, double defaultVal) {
		boolean sawAny = false;
		double out = Double.MAX_VALUE;
		for (Object o : selector.getVisibleObjects()) {
			if (o instanceof IdentifiedObject) {
				sawAny = true;
				double d = getExpressionValue(criterionID, defaultVal,
						(IdentifiedObject) o);
				if (d < out)
					out = d;
			}
		}
		if (sawAny)
			return out;
		else
			return defaultVal;
	}

	protected static double getMaxVal(ObjectSelector selector,
			String criterionID, double defaultVal) {
		boolean sawAny = false;
		double out = Double.MIN_VALUE;
		Collection c = selector.getVisibleObjects();
		for (Object o : c) {
			if (o instanceof IdentifiedObject) {
				sawAny = true;
				double d = getExpressionValue(criterionID, defaultVal,
						(IdentifiedObject) o);
				if (d > out)
					out = d;
			}
		}
		if (sawAny)
			return out;
		else
			return defaultVal;
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

	public Color getNoValueColor() {
		return noValueColor;
	}

	public void setNoValueColor(Color noValueColor) {
		this.noValueColor = noValueColor;
	}

	public String getMinValue() {
		return minValue;
	}

	public void setMinValue(String minValue) {
		this.minValue = minValue;
	}

	public String getMaxValue() {
		return maxValue;
	}

	public void setMaxValue(String maxValue) {
		this.maxValue = maxValue;
	}

	public String getCriterionID() {
		return criterionID;
	}

	public void setCriterionID(String criterionID) {
		this.criterionID = criterionID;
	}

	public boolean isDoBlend() {
		return doBlend;
	}

	public void setDoBlend(boolean doBlend) {
		this.doBlend = doBlend;
	}

	public HeatmapColor(Color minColor, Color maxColor, String criterionID) {
		this(minColor, maxColor, Color.white, "$min$", "$max$", criterionID,
				true);
	}

	public HeatmapColor(Color minColor, Color maxColor, Color noValueColor,
			String minValue, String maxValue, String criterionID,
			boolean doBlend) {
		super();
		this.minColor = minColor;
		this.maxColor = maxColor;
		this.noValueColor = noValueColor;
		this.minValue = minValue;
		this.maxValue = maxValue;
		this.criterionID = criterionID;
		this.doBlend = doBlend;
	}

	public String toString() {
		return "map values from criterion \"" + criterionID + "\" between "
				+ ColorUtil.getName(minColor) + " and "
				+ ColorUtil.getName(maxColor);
	}
}
