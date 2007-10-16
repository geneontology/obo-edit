package org.bbop.swing;

import javax.swing.*;
import java.awt.*;

public class DefaultEditorComponent extends JLabel implements GenericEditorComponent {
    /**
	 * 
	 */
	private static final long serialVersionUID = 7845226508902287594L;
	public void load(Object o) {
	setText(o.toString());
    }
    public void setMasterComponent(Component c) {}
    public void store(Object saveme) {}
    public void setEditable(boolean in) {}
    public Object createNewValue() {
	return null;
    }
}
