package org.oboedit.gui.filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.oboedit.controller.FilterManager;
import org.oboedit.gui.FilteredRenderable;

import org.apache.log4j.*;

public class GeneralRendererSpec implements RenderSpec {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GeneralRendererSpec.class);

	protected Map<GeneralRendererSpecField<?>, Object> map = new HashMap<GeneralRendererSpecField<?>, Object>();

	public GeneralRendererSpec() {
	}
	
	public Map<GeneralRendererSpecField<?>, Object> getMap() {
		return map;
	}
	
	public void setMap(Map<GeneralRendererSpecField<?>, Object> map) {
		this.map = map;
	}
	
	public GeneralRendererSpec(Object... vals) {
		if (vals.length % 2 == 1)
			throw new IllegalArgumentException(
					"Even number of arguments required.");
		for (int i = 0; i < vals.length / 2; i++) {
			GeneralRendererSpecField field = (GeneralRendererSpecField<?>) vals[i * 2];
			Object val = vals[i * 2 + 1];
			setValue(field, val);
		}
	}

	public <T> void setValue(GeneralRendererSpecField<T> field, T val) {
		if (val == null)
			map.remove(field);
		else
			map.put(field, val);
	}

	public <T> T getValue(GeneralRendererSpecField<T> field) {
		return (T) map.get(field);
	}

	public Collection<GeneralRendererSpecField<?>> getFields() {
		List<GeneralRendererSpecField<?>> fields = new ArrayList<GeneralRendererSpecField<?>>();
		fields.addAll(map.keySet());
		Collections.sort(fields, GeneralRendererSpecField.COMPARATOR);
		return fields;
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
			return null;
		}
	}

	public void clear() {
		map.clear();
	}

	public RenderSpec merge(FilteredRenderable fr, RenderSpec spec, Object o) {
		if (spec instanceof GeneralRendererSpec) {
			GeneralRendererSpec result = new GeneralRendererSpec();
			for (GeneralRendererSpecField field : FilterManager.getManager()
					.getRenderSpecFields()) {
				Object a = getValue(field);
				Object b = ((GeneralRendererSpec) spec).getValue(field);
				Object out;
				if (a == null)
					out = b;
				else if (b == null)
					out = a;
				else {
					out = field.merge(fr, a, b, o);
				}
				result.setValue(field, out);
			}
			return result;
		} else
			throw new IllegalArgumentException("Can only merge "
					+ "GeneralRendererSpec with another "
					+ "GeneralRendererSpec");
	}

	public String toString() {
		StringBuffer out = new StringBuffer();
		for (GeneralRendererSpecField field : getFields()) {
			if (out.length() > 0)
				out.append(", ");
			out.append("set " + field.getName() + " to " + getValue(field));
		}
		return out.toString();
	}

}
