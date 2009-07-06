package org.bbop.swing.widget;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import org.bbop.swing.*;

import java.util.*;

import org.apache.log4j.*;

public class FontChooser extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FontChooser.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 1000072409873550558L;
	JComboBox fontList;
    JComboBox sizeList;
    JComboBox styleList;

    JLabel fontLabel = new JLabel("Name");
    JLabel sizeLabel = new JLabel("Size");
    JLabel styleLabel = new JLabel("Style");

    protected int defaultSize = 12;
    protected Vector listeners = new Vector();
    protected boolean fireEvents = true;

    protected ActionListener actionListener = new ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		handleAction();
	    }
	};

    protected ListCellRenderer renderer = new DefaultListCellRenderer() {
	    /**
		 * 
		 */
		private static final long serialVersionUID = 2779389008687649515L;

		@Override
		public Component
	    getListCellRendererComponent(JList list,
					 Object value,
					 int index,
					 boolean isSelected,
					 boolean cellHasFocus) {
		Component c = super.getListCellRendererComponent(list,
								 value,
								 index,
								 isSelected,
								 cellHasFocus);
		int size = list.getFont().getSize();
		Font f = new Font((String) value,
				  getStyleForString((String) styleList.
						    getSelectedItem()),
				  size);
		c.setFont(f);
		return c;
	    }
	};

    public void addActionListener(ActionListener listener) {
	listeners.addElement(listener);
    }

    public void removeActionListener(ActionListener listener) {
	listeners.removeElement(listener);
    }

    protected void fireActionEvent(ActionEvent e) {
	for(int i=0; i < listeners.size(); i++) {
	    ActionListener listener = (ActionListener) listeners.elementAt(i);
	    listener.actionPerformed(e);
	}
    }

    public FontChooser() {
	this(false);
    }

    public FontChooser(boolean scaleableOnly) {
	super();
	String [] names = GraphicsEnvironment.getLocalGraphicsEnvironment().
	    getAvailableFontFamilyNames();
	if (scaleableOnly) {
	    Vector allowed = new Vector();
	    for(int i=0; i < names.length; i++) {
		Font f = new Font(names[i], 0, 10);
		if (SwingUtil.isVectorFont(f))
		    allowed.add(names[i]);
	    }
	    names = new String[allowed.size()];
	    allowed.copyInto(names);
	}
	String [] sizes = { "8", "10", "12", "14", "18", "24",
			    "36", "48" };
	String [] styles = {"normal", "italic", "bold", "bold-italic"};
	fontList = new JComboBox(names);
	sizeList = new JComboBox(sizes);
	sizeList.setEditable(true);
	styleList = new JComboBox(styles);
	Box nameBox = new Box(BoxLayout.Y_AXIS);
	Box sizeBox = new Box(BoxLayout.Y_AXIS);
	Box styleBox = new Box(BoxLayout.Y_AXIS);

	fontLabel.setAlignmentX(LEFT_ALIGNMENT);
	sizeLabel.setAlignmentX(LEFT_ALIGNMENT);
	styleLabel.setAlignmentX(LEFT_ALIGNMENT);

	fontList.setAlignmentX(LEFT_ALIGNMENT);
	sizeList.setAlignmentX(LEFT_ALIGNMENT);
	styleList.setAlignmentX(LEFT_ALIGNMENT);

	nameBox.add(fontLabel);
	nameBox.add(fontList);

	sizeBox.add(sizeLabel);
	sizeBox.add(sizeList);

	styleBox.add(styleLabel);
	styleBox.add(styleList);

	add(nameBox);
	add(sizeBox);
	add(styleBox);

	fontList.setRenderer(renderer);

	fontList.addActionListener(actionListener);
	sizeList.addActionListener(actionListener);
	styleList.addActionListener(actionListener);

	setFontListFont();
    }

    public void setDefaultFontSize(int size) {
	this.defaultSize = size;
    }

    public int getDefaultFontSize() {
	return defaultSize;
    }

    protected void setFontListFont() {
	int size = getFont().getSize();
	Font f = new Font((String) fontList.getSelectedItem(),
			  getStyleForString((String) styleList.
					    getSelectedItem()),
			  size);
	fontList.setFont(f);
    }

    protected void handleAction() {
	verify();
	setFontListFont();
	if (fireEvents)
	    fireActionEvent(new ActionEvent(this,
					    ActionEvent.ACTION_FIRST+200,
					    "FontChosen"));
    }

    protected void verify() {
	try {
	    int size = Integer.parseInt((String) sizeList.getSelectedItem());
	} catch (Exception e) {
	    selectOrAddFontSize(defaultSize);
	}
    }

    protected void selectOrAddFontSize(int newSize) {
	for(int i=0; i < sizeList.getItemCount(); i++) {
	    String size = (String) sizeList.getItemAt(i);
	    int sizeInt = -1;
	    try {
		sizeInt = Integer.parseInt(size);
	    } catch (NumberFormatException ex) {
		ex.printStackTrace();
		// this will never happen
	    }
	    if (sizeInt == newSize)
		break;
	    if (sizeInt > newSize) {
		styleList.insertItemAt(newSize+"", i);
	    }
	}
	sizeList.setSelectedItem(newSize+"");
    }

    protected String getStringForStyle(int style) {
	if (style == (Font.BOLD | Font.ITALIC))
	    return "bold-italic";
	else if (style == Font.BOLD)
	    return "bold";
	else if (style == Font.ITALIC)
	    return "italic";
	else
	    return "normal";
    }

    protected int getStyleForString(String styleName) {
	int style = Font.PLAIN;
	if (styleName.equals("bold"))
	    style = Font.BOLD;
	else if (styleName.equals("italic"))
	    style = Font.ITALIC;
	else if (styleName.equals("bold-italic"))
	    style = Font.BOLD | Font.ITALIC;
	return style;
    }

    public void setChosenFont(Font font) {
	fireEvents = false;
	fontList.setSelectedItem(font.getFamily());
	styleList.setSelectedItem(getStringForStyle(font.getStyle()));
	selectOrAddFontSize(font.getSize());
	fireEvents = true;
    }

    public Font getChosenFont() {
	int style = getStyleForString((String) styleList.getSelectedItem());
	int size = defaultSize;
	try {
	    size = Integer.parseInt((String) sizeList.getSelectedItem());
	} catch (Exception e) {
	}
	return new Font((String) fontList.getSelectedItem(),
			style, size);
			
    }

    @Override
	public void setFont(Font font) {
	super.setFont(font);
	if (sizeList != null)
	    sizeList.setFont(font);
	if (styleList != null)
	    styleList.setFont(font);
	if (fontLabel != null)
	    fontLabel.setFont(font);
	if (sizeLabel != null)
	    sizeLabel.setFont(font);
	if (styleLabel != null)
	    styleLabel.setFont(font);
    }
}
