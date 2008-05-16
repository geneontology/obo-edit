package org.oboedit.verify.impl;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;

import org.bbop.framework.GUIManager;
import org.bbop.swing.SwingUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.identifier.LinkIDResolution;
import org.obo.identifier.LinkIDWarning;
import org.obo.identifier.UnresolvedIDsException;
import org.obo.util.IDUtil;
import org.obo.util.ReasonerUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.components.IDResolutionComponent;
import org.oboedit.verify.AbstractCheck;
import org.oboedit.verify.CheckWarning;
import org.oboedit.verify.OntologyCheck;
import org.oboedit.verify.QuickFix;
import org.oboedit.verify.QuickFix.ReloadLevel;

import org.apache.log4j.*;

public class IDCheck extends AbstractCheck implements OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDCheck.class);

	public IDCheck() {
		// TODO Auto-generated constructor stub
	}

	public Collection<CheckWarning> check(OBOSession history,
			IdentifiedObject currentObject, byte condition,
			boolean checkObsoletes) {
		Set<CheckWarning> out = new LinkedHashSet<CheckWarning>();

		try {
			IDUtil.updateIDs(SessionManager.getManager().getSession(),
					new ArrayList<LinkIDResolution>(), false);
		} catch (UnresolvedIDsException e) {
			for (final LinkIDWarning w : e.getWarnings()) {
				Collection<QuickFix> fixes = new ArrayList<QuickFix>();
				if (w.getResolutions().size() > 0) {
					fixes.add(new AbstractImmediateQuickFix(
							"Show auto fix dialog...", ReloadLevel.GLOBAL) {
						public void run() {
							autofix(w);
						}
					});
				}

				CheckWarning warning = new CheckWarning(IDUtil.getDescription(
						w, false), false, this, w.getLink().getChild(), fixes);
				out.add(warning);
			}
		}

		return out;
	}

	protected void initConfiguration() {
		configuration.setCondition((byte) (VerificationManager.LOAD
				| VerificationManager.SAVE | VerificationManager.MANUAL));
	}

	@Override
	public String getDescription() {
		return "ID Check";
	}

	protected void autofix(LinkIDWarning w) {
		final JDialog dialog = new JDialog(GUIManager.getManager().getFrame(),
				true);
		final Collection<LinkIDResolution> res = new ArrayList<LinkIDResolution>();
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(IDResolutionComponent.getLine(w, res), BorderLayout.CENTER);
		JButton button = new JButton("Ok");
		button.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dialog.dispose();
				if (res.size() > 0) {
					try {
						IDUtil.updateIDs(SessionManager.getManager()
								.getSession(), res, true, true);
					} catch (UnresolvedIDsException ex) {
					}
					SessionManager.getManager().reload();
				}
			}
		});
		panel.add(button, BorderLayout.SOUTH);
		dialog.setContentPane(panel);
		dialog.pack();
		SwingUtil.center(GUIManager.getManager().getFrame(), dialog);
		dialog.show();
	}

	public String getID() {
		return "id_check";
	}

}
