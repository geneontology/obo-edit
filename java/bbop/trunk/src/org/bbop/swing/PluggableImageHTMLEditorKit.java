package org.bbop.swing;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.ImageView;

public class PluggableImageHTMLEditorKit extends HTMLEditorKit {

	public static interface IconFactory {
		public Icon createIcon(URL url, int width, int height);
	}

	protected Map<String, IconFactory> iconFactoryMap =
		new HashMap<String, IconFactory>();

	protected class PluggableImageHTMLFactory extends HTMLFactory {
		public View create(Element elem) {
			Object o = elem.getAttributes().getAttribute(
					StyleConstants.NameAttribute);
			if (o instanceof HTML.Tag) {
				HTML.Tag kind = (HTML.Tag) o;
				if (kind == HTML.Tag.IMG) {
					String url = getImageURL(elem).toString();
					int index = url.lastIndexOf('.');
					if (index > 0) {
						String suffix = url.substring(index+1, url.length());
						final IconFactory factory = iconFactoryMap.get(suffix);
						if (factory != null) {
							return new AbstractIconView(elem) {
								@Override
								public Icon createIcon(URL url, int width,
										int height) {
									return factory.createIcon(url, width, height);
								}
							};
						}
					}
				}
			}
			return super.create(elem);
		}
	}
	
	public void installFactory(String suffix, IconFactory factory) {
		iconFactoryMap.put(suffix, factory);
	}

	public ViewFactory getViewFactory() {
		return new PluggableImageHTMLFactory();
	}

	public static URL getImageURL(Element elem) {
		String src = (String) elem.getAttributes().getAttribute(
				HTML.Attribute.SRC);
		if (src == null) {
			return null;
		}

		URL reference = ((HTMLDocument) elem.getDocument()).getBase();
		try {
			URL u = new URL(reference, src);
			return u;
		} catch (MalformedURLException e) {
			return null;
		}
	}

}
