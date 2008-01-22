package org.oboedit.gui;

import javax.swing.JOptionPane;
import java.io.File;
import org.bbop.framework.GUIManager;

public class ExceptionHandler {
	public void handle(Throwable throwable) {
		if (throwable instanceof OutOfMemoryError)
			JOptionPane.showMessageDialog(null, "Out of memory!\n" +
					"OBO-Edit is out of memory! Please try\n"+
					"to backup your work, enter a higher memory setting\n"+
					"and restart OBO-Edit. Do not try to continue\n"+
					"working without restarting. OBO-Edit is in an\n"+
					"unstable state and may behave unpredictably.");
		else {
			throwable.printStackTrace();
			JOptionPane.showMessageDialog(null, "Unhandled exception!\n"+
					"This is always the result of a bug. Please go to\n" +
					"http://sourceforge.net/projects/geneontology and\n "+
					"submit a bug report. Please post a copy of the \n"+
					"stderr file located at\n"+
//					System.getProperty("user.home")+"/.oboedit/stderr");
						      GUIManager.getPrefsDir().getPath() + "stderr");
		}
	}
}
