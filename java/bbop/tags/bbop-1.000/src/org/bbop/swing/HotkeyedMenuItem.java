package org.bbop.swing;

import java.awt.*;
import javax.swing.*;

public class HotkeyedMenuItem extends JMenuItem {

    /**
	 * 
	 */
	private static final long serialVersionUID = 5832023922345037804L;
	protected String hotkeyDesc;

    public HotkeyedMenuItem(String name) {
	super(name);
    }

    public void setAccelerator(String key, String desc) {
	this.hotkeyDesc = desc;
	super.setAccelerator(KeyStroke.getKeyStroke(key));
    }
    
    public Component getComponent() {
	if (hotkeyDesc != null) {
	    Box out = Box.createHorizontalBox();
	    JLabel hotkeyLabel = new JLabel(hotkeyDesc);
	    hotkeyLabel.setHorizontalAlignment(JLabel.LEFT);
	    out.add(super.getComponent());
	    out.add(Box.createHorizontalGlue());
	    out.add(hotkeyLabel);
	    return out;
	} else
	    return super.getComponent();
    }
}
