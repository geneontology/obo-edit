package org.bbop.swing;

import org.bbop.expression.ExpressionException;

public interface XMLLayoutComponent {

    /**
     *
     * Updates the component in place. The component will re-evaluate
     * any expressions and relayout its contents
     *
     */
    public void guiupdate() throws ExpressionException ;
}
