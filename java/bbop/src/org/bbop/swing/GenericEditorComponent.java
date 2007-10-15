package org.bbop.swing;

import java.awt.Component;

public interface GenericEditorComponent {
    public void setMasterComponent(Component c);
    public void load(Object o);
    public void store(Object saveme);
    public Object createNewValue();
}
