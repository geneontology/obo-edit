package org.bbop.swing;

import java.awt.Window;

import org.bbop.expression.ExpressionException;

public interface XMLLayoutRoot extends XMLLayoutComponent {

    /**
     * Completely reparses the layout
     */
    public boolean reload();
    public Exception getParseException();
    public boolean setXMLLayout(XMLLayout layout);
    public XMLLayout getXMLLayout();
    public void setIsLayoutRoot(boolean isLayoutRoot);
    public void minimizeWindow(Window window);
}
