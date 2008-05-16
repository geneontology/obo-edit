package org.oboedit.gui.actions;

import org.bbop.framework.GUIManager;

import java.awt.BorderLayout;
import java.util.*;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.DanglingLinkImpl;
import org.obo.history.*;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;

import javax.swing.border.TitledBorder;
import javax.swing.tree.TreePath;
import javax.swing.*;

import jwf.Wizard;
import jwf.WizardListener;
import jwf.WizardPanel;

import org.apache.log4j.*;

public class MultiAddAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiAddAction.class);

	protected LinkedObject target;

	protected Selection selection;

	protected GestureTarget dest;

	protected boolean isLegal = false;

	protected JSpinner spinner = new JSpinner();

	protected JTextArea nameArea = new JTextArea();

	protected List<String> nameList = new LinkedList<String>();

	protected class NameEntryPanel extends WizardPanel {

		public NameEntryPanel() {
			setLayout(new BorderLayout());
			add(new JLabel("Enter the new term names, one per line"), "North");
			add(new JScrollPane(nameArea,
					JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
					JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), "Center");
		}

		@Override
		public boolean canFinish() {
			return true;
		}

		@Override
		public void display() {
			nameArea.setText("");
		}

		@Override
		public void finish() {
			nameList.clear();
			StringTokenizer tokenizer = new StringTokenizer(nameArea.getText(),
					"\n");
			while (tokenizer.hasMoreTokens()) {
				String name = tokenizer.nextToken().trim();
				if (name.length() > 0)
					nameList.add(name);
			}
		}

		@Override
		public boolean hasNext() {
			return false;
		}

		@Override
		public WizardPanel next() {
			return null;
		}

		@Override
		public boolean validateFinish(List arg) {
			return true;
		}

		@Override
		public boolean validateNext(List arg) {
			return true;
		}

	}

	protected class NumberPickerPanel extends WizardPanel {

		public NumberPickerPanel() {
			JPanel controlPanel = new JPanel();
			controlPanel.setLayout(new BorderLayout());
			setLayout(new BorderLayout());
			controlPanel.add(new JLabel("How many terms should be created?"),
					"Center");
			controlPanel.add(spinner, "East");
			add(controlPanel, "North");
		}

		@Override
		public boolean canFinish() {
			return true;
		}

		@Override
		public void display() {
			spinner.setValue(new Integer(1));
		}

		@Override
		public void finish() {
			nameList.clear();
			if (spinner.getModel() instanceof SpinnerNumberModel) {
				int val = ((SpinnerNumberModel) spinner.getModel()).getNumber()
						.intValue();
				for (int i = 0; i < val; i++)
					nameList.add("<new term " + (i + 1) + ">");
			}
		}

		@Override
		public boolean hasNext() {
			return false;
		}

		@Override
		public WizardPanel next() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public boolean validateFinish(List arg0) {
			return true;
		}

		@Override
		public boolean validateNext(List arg0) {
			// TODO Auto-generated method stub
			return true;
		}

	}

	protected class MainWizardPanel extends WizardPanel {

		// private final WizardPanel license = new LicenseWizardPanel();
		protected JRadioButton numberButton = new JRadioButton(
				"Automatically generate a certain number of child terms");

		protected JRadioButton nameButton = new JRadioButton(
				"Type in a list of new child names", true);

		public MainWizardPanel() {
			ButtonGroup group = new ButtonGroup();
			group.add(numberButton);
			group.add(nameButton);
			setBorder(new TitledBorder("Choose Mode"));
			setLayout(new BorderLayout());
			JLabel titleLabel = new JLabel(
					"Please choose how new terms should be created");
			JPanel controlPanel = new JPanel();
			controlPanel
					.setLayout(new BoxLayout(controlPanel, BoxLayout.Y_AXIS));
			controlPanel.add(numberButton);
			controlPanel.add(nameButton);
			add(titleLabel, "North");
			add(controlPanel, "Center");
		}

		/** Called when the panel is set. */
		public void display() {
		}

		/**
		 * Is there be a next panel?
		 * 
		 * @return true if there is a panel to move to next
		 */
		public boolean hasNext() {
			return true;
		}

		/**
		 * Called to validate the panel before moving to next panel.
		 * 
		 * @param list
		 *            a List of error messages to be displayed.
		 * @return true if the panel is valid,
		 */
		public boolean validateNext(List list) {
			return true;
		}

		/** Get the next panel to go to. */
		public WizardPanel next() {
			if (numberButton.isSelected())
				return new NumberPickerPanel();
			else
				return new NameEntryPanel();
		}

		/**
		 * Can this panel finish the wizard?
		 * 
		 * @return true if this panel can finish the wizard.
		 */
		public boolean canFinish() {
			return true;
		}

		/**
		 * Called to validate the panel before finishing the wizard. Should
		 * return false if canFinish returns false.
		 * 
		 * @param list
		 *            a List of error messages to be displayed.
		 * @return true if it is valid for this wizard to finish.
		 */
		public boolean validateFinish(List list) {
			return true;
		}

		/** Handle finishing the wizard. */
		public void finish() {
		}
	}

	public KeyStroke getKeyStroke() {
		return null;
	}

	public String getName() {
		return "Create multiple child terms...";
	}

	public String getDesc() {
		return "Create multiple child terms...";
	}

	public List getSubActions() {
		return null;
	}

	public void clickInit(Selection paths, GestureTarget destItem) {
		isLegal = false;
		if (paths.getTerms().size() == 1) {
			selection = paths;
			target = paths.getTerms().iterator().next();
			dest = SelectionManager.createGestureTarget(paths.getComponent(),
					paths.getPath(), true);
			if (!TermUtil.isObsolete(target) && dest != null) {
				isLegal = true;
			}
		}
	}

	public boolean isLegal() {
		return isLegal;
	}

	public HistoryItem execute() {
		nameList.clear();
		Map<String, String> names = new HashMap<String, String>();
		names.put(Wizard.BACK_I18N, "Back");
		names.put(Wizard.NEXT_I18N, "Next");
		names.put(Wizard.FINISH_I18N, "Finish");
		names.put(Wizard.HELP_I18N, "Help");
		names.put(Wizard.CANCEL_I18N, "Cancel");
		final JDialog frame = new JDialog(GUIManager.getManager().getFrame(),
				true);
		Wizard wizard = new Wizard();
		wizard.setI18NMap(names);

		wizard.addWizardListener(new WizardListener() {
			/**
			 * Called when the wizard finishes.
			 * 
			 * @param wizard
			 *            the wizard that finished.
			 */
			public void wizardFinished(Wizard wizard) {
				frame.dispose();
			}

			/**
			 * Called when the wizard is cancelled.
			 * 
			 * @param wizard
			 *            the wizard that was cancelled.
			 */
			public void wizardCancelled(Wizard wizard) {
				frame.dispose();
			}

			/**
			 * Called when a new panel has been displayed in the wizard.
			 * 
			 * @param wizard
			 *            the wizard that was updated
			 */
			public void wizardPanelChanged(Wizard wizard) {
			}
		});
		frame.setContentPane(wizard);
		frame.pack();
		wizard.start(new MainWizardPanel());
		frame.setVisible(true);
		if (nameList.size() >= 1) {
			String[] ids = IDUtil.fetchIDs(IDManager.getManager()
					.getIDAdapter(), SessionManager.getManager().getSession(),
					dest.getTerm(), nameList.size());
			TermMacroHistoryItem item = new TermMacroHistoryItem("Create terms");
			for (int i = 0; i < ids.length; i++) {
				item.addItem(new CreateObjectHistoryItem(ids[i],
						OBOClass.OBO_CLASS.getID()));
				item.addItem(new CreateLinkHistoryItem(ids[i], OBOProperty.IS_A
						.getID(), dest.getTerm().getID()));
				item.addItem(new NameChangeHistoryItem(nameList.get(i), ids[i],
						ids[i]));
				item.addItem(new NamespaceHistoryItem(null, dest.getTerm().getNamespace(), ids[i]));
			}
			GUIUtil.setPreSelection(item, selection);
			TreePath[] postPaths = new TreePath[ids.length];
			for (int i = 0; i < postPaths.length; i++) {
				postPaths[i] = dest.getPath().pathByAddingChild(
						new DanglingLinkImpl(ids[i], OBOProperty.IS_A.getID(),
								target.getID()));
			}
			GUIUtil.setPostSelection(item, SelectionManager.createSelectionFromPaths(
					null, postPaths, null, SessionManager.getManager()
							.getCurrentLinkDatabase(), RootAlgorithm.GREEDY,
					true));
			return item;
		} else
			return null;
	}
}
