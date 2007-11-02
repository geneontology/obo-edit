package org.oboedit.gui.filter;

import java.util.Comparator;

public interface GeneralRendererSpecField<T> {

	public static final int NON_HTML = 0;
	public static final int INNER_HTML = 1;
	public static final int HTML = 2;
	public static final int OUTER_HTML = 4;

	public static Comparator<GeneralRendererSpecField<?>> COMPARATOR = new Comparator<GeneralRendererSpecField<?>>() {

		public int compare(GeneralRendererSpecField<?> o1, GeneralRendererSpecField<?> o2) {
			int a = o1.getHTMLType();
			int b = o2.getHTMLType();
			return a - b;
		}
	};

	public T merge(T a, T b);

	public GeneralRendererSpecFieldEditor<T> getEditor();

	public String getID();

	public String getName();

	public int getHTMLType();

	public void renderHTML(T value, StringBuffer in);
	
	public boolean isObjectRenderer();
	
	public boolean isLinkRenderer();
}
