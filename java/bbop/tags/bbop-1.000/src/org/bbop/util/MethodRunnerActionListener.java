/****
 * MethodRunnerActionListener
 *
 * Runs the named method when the actionEvent fires
 *
 * @deprecated This class cannot be type checked at compile time. It is
 * always preferable to use an anonymous ActionListener class than this
 * class
 */

package org.bbop.util;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.lang.reflect.*;

public class MethodRunnerActionListener implements ActionListener {
    Object target;
    String methodName;
    Method method;

    /**
     * methodName is the name of the method to run (no parens)
     *    (the method must not take any args)
     * target is the object to run the method on
     *
     * if the method does not exist for that object, an exception is thrown
     *
     * @deprecated Do not use this class. Use an anonymous ActionListener
     * class instead to enable compile time checking.
     */
    public MethodRunnerActionListener(Object target, String methodName) throws NoSuchMethodException {
	this.target = target;
	method = target.getClass().getMethod(methodName, new Class[0]);
    }

    /**
     * Runs the method named in the constructor. If the method throws an
     * exception, it's stack trace will be printed, but the exception will
     * not be propegated
     *
     * It should not be possible for the illegal access exception or
     * IllegalArgumentException methods to be thrown
     * (any IllegalAccessException should have happened on construction, and
     *  the method must be a zero parameter method, so there can't be an
     *  IllegalArgumentException)
     * If they are somehow thrown, a stack trace is printed and the exception
     * is not propegated
     *
     * @deprecated Do not use this class. Use an anonymous ActionListener
     * class instead to enable compile time checking.
     */
    public void actionPerformed(ActionEvent evt) {
	try {
	    method.invoke(target, new Object[0]);
	} catch (InvocationTargetException e) {
	    e.getTargetException().printStackTrace();
	} catch (IllegalAccessException e) {
	    System.err.println("Unexpected condition, IllegalAccessException");
	    e.printStackTrace();
	} catch (IllegalArgumentException e) {
	    System.err.println("Unexpected condition, IllegalArgumentException");
	    e.printStackTrace();
	}
    }
}
