package org.bbop.swing;

import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.net.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.util.*;

public class XMLMenuBar extends JMenuBar {

    /**
	 * 
	 */
	private static final long serialVersionUID = 8345061479714901147L;
	protected Vector items = new Vector();
    protected MenubarElement root;

    protected static interface Holder {
	public void addItem(Object element);
	public Vector getItems();
    }

    protected static class MenubarElement implements Holder {
	Vector items;

	public MenubarElement() {
	    items = new Vector();
	}

	public void addItem(Object item) {
	    items.add(item);
	}

	public Vector getItems() {
	    return items;
	}
    }

    protected static class MenuElement implements Holder {
	Vector items;
	String label;

	public MenuElement(String label) {
	    items = new Vector();
	    this.label = label;
	}

	public void addItem(Object item) {
	    items.add(item);
	}

	public Vector getItems() {
	    return items;
	}

	public String getLabel() {
	    return label;
	}
    }

    protected static class SeparatorElement {
    }

    protected static class UserItem {
	protected String id;
	protected Properties props;

	public UserItem(String id, Properties props) {
	    this.id = id;
	    this.props = props;
	}

	public String getID() {
	    return id;
	}

	public Properties getProperties() {
	    return props;
	}
    }

    protected MenuListener showListener = new MenuListener() {
	    public void menuSelected(MenuEvent e) {
		resolver.showNotify();
	    }

	    public void menuCanceled(MenuEvent e) {
	    }

	    public void menuDeselected(MenuEvent e) {
	    }
	};

    protected MenuNameResolver resolver;

    public XMLMenuBar(MenuNameResolver resolver) {
	this.resolver = resolver;
    }

    protected void reload(MenubarElement root) {
	removeAll();
	items.removeAllElements();
	for(int i=0; i < root.getItems().size(); i++) {
	    Component component = getComponent(root.getItems().get(i));
	    add(component);
	}
	validate();
	repaint();
    }

    protected Component getComponent(Object o) {
	if (o instanceof MenuElement) {
	    JMenu menu = getMenu((MenuElement) o);
	    return menu;
	} else if (o instanceof UserItem) {
	    UserItem item = (UserItem) o;
	    Component comp = resolver.resolveName(item.getID(),
						  item.getProperties(),
						  null);
	    return comp;
	} else {
	    return null;
	}
    }

    protected JMenu getMenu(MenuElement element) {
	JMenu menu = new JMenu(element.getLabel());
	menu.setFont(getFont());
	menu.addMenuListener(showListener);
	for(int i=0; i < element.getItems().size(); i++) {
	    Object o = element.getItems().get(i);
	    if (o instanceof SeparatorElement)
		menu.addSeparator();
	    else
		menu.add(getComponent(o));
	}
	return menu;
    }

    public void setFont(Font font) {
	super.setFont(font);
	if (root != null)
	    reload(root);
    }


    public void setLayout(String layout) throws SAXException, IOException {
	setLayout(new InputSource(new StringReader(layout)));
    }

    public void setLayout(URL url) throws SAXException, IOException {
	setLayout(url.openStream());
    }

    public void setLayout(File file) throws SAXException, IOException {
	setLayout(file.toURL());
    }

    public void setLayout(InputStream stream) throws SAXException,
	IOException {
	setLayout(new InputSource(stream));
    }

    public void setLayout(InputSource document)
	throws SAXException, IOException {
	// Create a JAXP SAXParserFactory and configure it
	SAXParserFactory spf = SAXParserFactory.newInstance();
	spf.setValidating(false);
	    
	XMLReader xmlReader = null;

	// Create a JAXP SAXParser
	SAXParser saxParser;

	try {
	    saxParser = spf.newSAXParser();
	} catch (ParserConfigurationException e) {
	    throw new IOException("Couldn't load parser");
	}
	    
	// Get the encapsulated SAX XMLReader
	xmlReader = saxParser.getXMLReader();

	LayoutBuilder layoutBuilder = new LayoutBuilder();
	    
	// Set the ContentHandler of the XMLReader
	xmlReader.setContentHandler(layoutBuilder);
	resolver.startParseNotify();
	xmlReader.parse(document);
	resolver.endParseNotify();
	root = layoutBuilder.getRoot();

	reload(root);
    }

    protected static class LayoutBuilder extends DefaultHandler {

	private Vector stack;
	private MenubarElement root;

	public MenubarElement getRoot() {
	    return root;
	}

	// Parser calls this once at the beginning of a document
	public void startDocument() throws SAXException {
	    stack = new Vector();
	}

	// Parser calls this for each element in a document
	public void startElement(String namespaceURI, String localName,
				 String rawName, Attributes atts)
	    throws SAXException
	{
	    Object item = null;
	    localName = rawName;
	    if (localName.equalsIgnoreCase("component")) {
		Properties props = new Properties();
		for(int i=0; i < atts.getLength(); i++) {
		    String name = atts.getQName(i);
		    String value = atts.getValue(i);
		    if (!name.equals("id"))
			props.put(name, value);
		}
		item = new UserItem(atts.getValue("id"), props);
		stack.insertElementAt(item,0);
	    } else if (localName.equalsIgnoreCase("menubar")) {
		item = new MenubarElement();
		stack.insertElementAt(item,0);
	    } else if (localName.equalsIgnoreCase("separator")) {
		item = new SeparatorElement();
		stack.insertElementAt(item,0);
            } else if(localName.equalsIgnoreCase("menu")) {
                item = new MenuElement(atts.getValue("label"));
                stack.insertElementAt(item, 0);
	    }

	    if (root == null && item != null && item instanceof MenubarElement)
		root = (MenubarElement) item;
	}

	public void endElement(java.lang.String uri,
			       java.lang.String localName,
			       java.lang.String qName) {
	    if (stack.size() >= 2) {
		Object item = stack.elementAt(0);
		Holder parent = (Holder) stack.elementAt(1);
		parent.addItem(item);
		stack.removeElementAt(0);
	    }
	}
	
	// Parser calls this once after parsing a document
	public void endDocument() throws SAXException {

	}
    }
}



