package org.oboedit.gui.components.ontologyGeneration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URLEncoder;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.JTextComponent;

import org.apache.axis2.AxisFault;
import org.apache.log4j.Logger;
import org.jdesktop.swingworker.SwingWorker;
import org.oboedit.gui.components.ontologyGeneration.bioportal.ParseXMLFile;
import org.oboedit.gui.components.ontologyGeneration.extraction.DefinitionExtensionWorker;
import org.oboedit.gui.components.ontologyGeneration.extraction.PdfToTextExtraction;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTable;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyClassInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyGenerationComponentServiceInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface;
import org.oboedit.gui.components.ontologyGeneration.interfaces.ParentRelationEntry;

import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub;
import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub.DefinitionContainer;
import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub.GetDefinitionsForLanguage;
import de.tud.biotec.gopubmedDefinitionGeneration.client.GoPubMedDefinitionGeneratorStub.GetDefinitionsForLanguageResponse;
import de.tud.biotec.gopubmedOntologyLookupService.OntologyLookupManagerPortTypeProxy;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupRelation;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupResult;
import de.tud.biotec.gopubmedOntologyLookupService.xsd.OBOLookupTerm;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQuery;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQueryResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromTextForLanguage;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromTextForLanguageResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromWebQueryForLanguage;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateConceptsFromWebQueryForLanguageResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateSiblingsFromWeb;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.GenerateSiblingsFromWebResponse;
import de.tud.biotec.gopubmedTermGenerationService.client.GoPubMedTermGenerationStub.TextConceptRepresentation;

/**
 * Ontology Generation Component which supports the automatic generation of
 * candidate terms and candidate definitions to create candidate to be added to
 * the loaded ontologies.
 * <p>
 * Developed at the Bioinformatics Group, BIOTEC, TU Dresden, Dresden, Germany
 * </p>
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 * @version 2.0, 25/06/2009
 */
public abstract class OntologyGenerationComponent<T extends OntologyClassInterface, R> implements
		PropertyChangeListener, OntologyGenerationComponentServiceInterface<T, R> {

	private final OntologyModelAdapterInterface<T, R> adapter;
	private final JComponent guiComponent;

	public static final String PLUGIN_VERSION = "3.3";
	public String PLUGIN_VERSIONED_NAME = null;
	private static final String SOURCE_PUBMED = "PUBMED"; //$NON-NLS-1$
	private static final String SOURCE_TEXT = "TEXT"; //$NON-NLS-1$
	private static final String SOURCE_FOLDER = "FOLDER"; //$NON-NLS-1$
	private static final String SOURCE_WEB = "WEB"; //$NON-NLS-1$
	private static final String SOURCE_SIBLING = "SIBLING"; //$NON-NLS-1$
	// private final Color COLOR_FILTER_FIELDS = new Color(255, 255, 180);
	private final Color COLOR_TERMS_TABLE = new Color(255, 255, 240);
	private final Color COLOR_DEF_TABLE = new Color(255, 255, 240);
	private final Color COLOR_OBOTERMS_TABLE = new Color(255, 255, 240);
	// private final Color COLOR_TERMS_TABLE = new Color(255, 255, 240);
	// private final Color COLOR_DEF_TABLE = new Color(255, 240, 240);
	// private final Color COLOR_OBOTERMS_TABLE = new Color(230, 255, 230);

	private static final Logger logger = Logger.getLogger(OntologyGenerationComponent.class);

	// private String wordInclusionPattern =
	// "^<X>[\\s|-|\\.|,|;]|[\\s|-|\\.|,|;]<X>[\\s|-|\\.|,|;]|[\\s|-|\\.|,|;]<X>$";

	// Variables
	private CandidateTerm selectedCandidateTerm;
	private List<OntologyClassInterface> tentativeSeedTerms;
	private String id;

	// Tables
	private TermsTable candidateTermsTable;
	private DefinitionsTable definitionTable;
	private AbstractOntologyTermsTable<T, R> ontologyTermsTable;

	// GUI related
	private ProgressBarDialog progressDlg;
	private JButton generateTermsFromPubMedButton;
	private JButton generateTermsFromWebButton;
	private JButton generateSiblingsButton;
	private JButton generateTermsFromTextButton;
	private JButton generateTermsFromFolderButton;
	private JButton generateManualDefinitionButton;
	private JButton folderChooseButton;
	private JButton showClipboardButton;
	private JButton clearClipboardButton;
	private JButton loadClipboardButton;
	private JButton saveClipboardButton;

	private JButton saveLabelButton;
	private JLabel saveLabelWarningLabel;
	private JButton saveDefButton;
	private JButton deleteDefButton;
	private JLabel saveDefWarningLabel;
	private JButton addToOntologyButton;

	private JCheckBox checkboxShowTickedParents;
	private JCheckBox onlyShowExistingTerms = new JCheckBox();

	private JTextArea inputTextArea;
	private JTextArea editDefArea;

	private JTextField inputPubMedQueryField;
	private JTextField inputWebQueryField;
	private JTextField inputSeedTermField1;
	private JTextField inputSeedTermField2;
	private JTextField inputSeedTermField3;
	private JTextField inputFolderLocationField;
	private FilterTextField searchTermsTextField;
	private FilterTextField searchDefTextField;
	private JTextField inputDefinitionGenerationField;
	private JTextField editNameTextField = new JTextField(20);
	private JTextField selectedLinkedObjectField = new JTextField(10);
	private FilterTextField searchPotentialParentsTextField;
	private JComboBox languageSelectionBox;
	private JComboBox ontologyLookupSelectionBox;

	private JTabbedPane clipboardTabPanel;
	private JScrollPane scrollPaneForTermsTable;
	private JScrollPane scrollPaneForDefinitionsTable;

	// Caches and clipboard
	private static CandidateTermCache clipboard = new CandidateTermCache();
	private CandidateTermCache candidateTermCache = new CandidateTermCache();

	// Web service related
	private BlockingQueue<CandidateTerm> ontologyLookupQueue;
	private BlockingQueue<CandidateTerm> ontologyLookupChildrenQueue;
	private static GoPubMedTermGenerationStub termGenerationServiceStub;
	private static OntologyLookupManagerPortTypeProxy ontoLookupProxy;
	private static GoPubMedTermGenerationStub siblingGenerationServiceStub;
	private static GoPubMedDefinitionGeneratorStub definitionGeneratorStub;

	private JCheckBox checkboxIncludeChildren;
	// private JCheckBox checkboxIncludeBranch;

	private BioPortalLookupServiceWorker bioPortalLookupServiceWorker;
	private OntologyLookupServiceWorker lookupServiceWorker;
	private OntologyLookupChildrenServiceWorker lookupChildrenServiceWorker;
	private JPanel termGenerationPanel;
	private JPanel definitionGenerationPanel;
	private JPanel addToOntologyPanel;

	private int selectedParentTermRow = -1;
	private String lastSource;

	private SortedMap<String, Locale> ontologyLocales;

	private JTextField proxyHostTextField;
	private JTextField proxyPortTextField;
	private JTextField proxyUserTextField;
	private JPasswordField proxyPasswordField;
	private JButton proxyResetButton;
	private JButton proxySaveButton;

	// Favicons
	private BlockingQueue<CandidateDefinition> faviconRetrievalQueue;
	private FaviconRetrieverWorker faviconRetrieverWorker;
	private BitSet faviconBitSet;

	/*
	 * ABSTRACT METHODS
	 */
	public abstract AbstractOntologyTermsTable<T, R> createOntologyTermsTable();

	/**
	 * Constructs a {@link OntologyGenerationComponent} instance
	 * 
	 * @param id
	 */
	public OntologyGenerationComponent(OntologyModelAdapterInterface<T, R> adapter, JComponent guiComponent) {
		// this.id = id;
		this.adapter = adapter;
		adapter.registerForOntologyTermSelectionChange(this);
		PLUGIN_VERSIONED_NAME = adapter.getOntologyEditorVersion() + "_" + PLUGIN_VERSION; //$NON-NLS-1$
		this.guiComponent = guiComponent;
		this.ontologyLookupQueue = new LinkedBlockingQueue<CandidateTerm>(100) {
			private static final long serialVersionUID = 7448640043596470248L;
		};
		this.ontologyLookupChildrenQueue = new LinkedBlockingQueue<CandidateTerm>(3) {
			private static final long serialVersionUID = 1L;
		};

		// TERMS TABLE
		this.candidateTermsTable = new TermsTable(adapter, clipboard, 4, true);
		this.candidateTermsTable.setBackground(COLOR_TERMS_TABLE);
		this.candidateTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		candidateTermsTable.updateTermsTableStructure();

		// DEFINITIONS TABLE
		this.definitionTable = new DefinitionsTable();
		this.definitionTable.setBackground(COLOR_DEF_TABLE);
		if (this.definitionTable.getColumnCount() > 2) {
			TableCellImageRenderer termInformationIconRenderer = new TableCellImageRenderer("resources/aboutIcon.png"); //$NON-NLS-1$
			termInformationIconRenderer.setToolTipText(Messages
					.getString("OntologyGenerationComponent.OpenDefinitionsPopupImage.tooltip")); //$NON-NLS-1$
			this.definitionTable.getColumnModel().getColumn(3).setCellRenderer(termInformationIconRenderer);
		}
		this.faviconRetrievalQueue = new LinkedBlockingQueue<CandidateDefinition>(100) {
			private static final long serialVersionUID = -7071083430934417609L;
		};
		this.faviconBitSet = new BitSet();

		// OBO TERMS TABLE
		ontologyTermsTable = createOntologyTermsTable();
		ontologyTermsTable.setBackground(COLOR_OBOTERMS_TABLE);
		ontologyTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		// oboTermsTable.setPreferredScrollableViewportSize(new Dimension(200,
		// 200));
		ontologyTermsTable.setMinimumPreferedeScrollableViewportHeight(100);
		ontologyTermsTable.setMaximumPreferedeScrollableViewportHeight(300);
		TableCellImageRenderer openInOntologyRenderer = new TableCellImageRenderer("resources/openIcon.png"); //$NON-NLS-1$
		openInOntologyRenderer.setToolTipText(Messages
				.getString("OntologyGenerationComponent.OpenTermInDefintionPopupImage.tooltip")); //$NON-NLS-1$
		ontologyTermsTable.getColumnModel().getColumn(1).setCellRenderer(openInOntologyRenderer);

		this.selectedCandidateTerm = null;
		this.tentativeSeedTerms = new ArrayList<OntologyClassInterface>();

		this.inputPubMedQueryField = new JTextField();
		this.inputPubMedQueryField.setMaximumSize(new Dimension(1000, 25));
		this.inputPubMedQueryField.setPreferredSize(new Dimension(100, 25));

		this.inputWebQueryField = new JTextField();
		this.inputWebQueryField.setMaximumSize(new Dimension(1000, 25));
		this.inputWebQueryField.setPreferredSize(new Dimension(100, 25));

		this.inputSeedTermField1 = new JTextField();
		this.inputSeedTermField1.setMinimumSize(new Dimension(30, 25));
		this.inputSeedTermField1.setMaximumSize(new Dimension(250, 25));
		this.inputSeedTermField1.setPreferredSize(new Dimension(30, 25));
		this.inputSeedTermField2 = new JTextField();
		this.inputSeedTermField2.setMinimumSize(new Dimension(30, 25));
		this.inputSeedTermField2.setMaximumSize(new Dimension(250, 25));
		this.inputSeedTermField2.setPreferredSize(new Dimension(30, 25));
		this.inputSeedTermField3 = new JTextField();
		this.inputSeedTermField3.setMinimumSize(new Dimension(30, 25));
		this.inputSeedTermField3.setMaximumSize(new Dimension(250, 25));
		this.inputSeedTermField3.setPreferredSize(new Dimension(30, 25));

		this.inputFolderLocationField = new JTextField();
		this.inputFolderLocationField.setMaximumSize(new Dimension(1000, 25));
		this.inputFolderLocationField.setPreferredSize(new Dimension(100, 25));
		// this.inputFolderLocationField.setEditable(false);

		this.generateTermsFromPubMedButton = new JButton(
				Messages.getString("OntologyGenerationComponent.GenerateTermsFromPubMedButton.label")); //$NON-NLS-1$
		this.generateTermsFromWebButton = new JButton(
				Messages.getString("OntologyGenerationComponent.GenerateTermsFromWebButton.label")); //$NON-NLS-1$
		this.generateSiblingsButton = new JButton(
				Messages.getString("OntologyGenerationComponent.GenerateSiblingsButton.label")); //$NON-NLS-1$
		this.generateTermsFromTextButton = new JButton(
				Messages.getString("OntologyGenerationComponent.GenerateTermsFromTextButton.label")); //$NON-NLS-1$
		this.generateTermsFromFolderButton = new JButton(
				Messages.getString("OntologyGenerationComponent.GenerateTermsFromFolderButton.label")); //$NON-NLS-1$
		this.generateTermsFromFolderButton.setEnabled(false);
		this.generateManualDefinitionButton = new JButton(
				Messages.getString("OntologyGenerationComponent.GenerateManualDefinitionButton.label")); //$NON-NLS-1$
		this.folderChooseButton = new JButton(
				Messages.getString("OntologyGenerationComponent.FolderChooseButton.label")); //$NON-NLS-1$

		// clipboard
		this.showClipboardButton = new JButton(
				Messages.getString("OntologyGenerationComponent.ShowClipboardButton.label")); //$NON-NLS-1$
		this.clearClipboardButton = new JButton(
				Messages.getString("OntologyGenerationComponent.ClearClipboardButton.label")); //$NON-NLS-1$
		this.loadClipboardButton = new JButton(
				Messages.getString("OntologyGenerationComponent.LoadClipboardButton.label")); //$NON-NLS-1$
		this.saveClipboardButton = new JButton(
				Messages.getString("OntologyGenerationComponent.SaveClipboardButton.label")); //$NON-NLS-1$

		this.saveDefButton = new JButton(Messages.getString("OntologyGenerationComponent.SaveDefButton.label")); //$NON-NLS-1$
		this.saveLabelButton = new JButton(Messages.getString("OntologyGenerationComponent.SaveLabelButton.label")); //$NON-NLS-1$

		this.deleteDefButton = new JButton(
				Messages.getString("OntologyGenerationComponent.DeleteDefinitionButton.label")); //$NON-NLS-1$
		this.deleteDefButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.DeleteDefinitionButton.tooltip")); //$NON-NLS-1$

		// TOOLTIPS
		this.generateTermsFromPubMedButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.GenerateTermsFromPubMedButton.tooltip")); //$NON-NLS-1$
		this.generateTermsFromWebButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.GenerateTermsFromWebButton.tooltip")); //$NON-NLS-1$
		this.generateSiblingsButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.GenerateSiblingsButton.tooltip")); //$NON-NLS-1$)
		this.generateTermsFromTextButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.GenerateTermsFromTextButton.tooltip")); //$NON-NLS-1$
		this.generateTermsFromFolderButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.GenerateTermsFromFolderButton.tooltip")); //$NON-NLS-1$
		this.generateManualDefinitionButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.GenerateDefinitionsButton.tooltip")); //$NON-NLS-1$
		this.showClipboardButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.ShowClipboardButton.tooltip")); //$NON-NLS-1$
		this.clearClipboardButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.ClearClipboardButton.tooltip")); //$NON-NLS-1$
		this.loadClipboardButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.LoadClipboardButton.tooltip")); //$NON-NLS-1$
		this.saveClipboardButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.SaveClipboardButton.tooltip")); //$NON-NLS-1$

		this.saveDefButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.SaveDefinitionsButton.tooltip")); //$NON-NLS-1$
		this.saveLabelButton.setToolTipText(Messages.getString("OntologyGenerationComponent.SaveLabelButton.tooltip")); //$NON-NLS-1$

		this.saveDefWarningLabel = new JLabel();
		this.saveLabelWarningLabel = new JLabel();

		ontologyLocales = new TreeMap<String, Locale>();
		setUpOntologyLanguagesSelectionBox();
		setUpOntologyLookupSelectionBox();
	}

	public void initListener() {
		try {
			attachListeners();
		} catch (Exception exception) {
			throw new RuntimeException(exception);
		}
	}

	public void initClipboardAndWorker() {
		// init clipboard
		if (clipboard.getSize() > 0) {
			updateClipboard(candidateTermsTable);
		}

		// start worker and services
		initOntologyLookupServices();

		faviconRetrieverWorker = new FaviconRetrieverWorker();
		faviconRetrieverWorker.execute();
	}

	private void initOntologyLookupServices() {
		if (lookupServiceWorker != null) {
			logger.info("Cancel EBI ontology lookup service");
			lookupServiceWorker.cancel(true);
		}
		if (bioPortalLookupServiceWorker != null) {
			logger.info("Cancel BioPortal ontology lookup service");
			bioPortalLookupServiceWorker.cancel(true);
		}
		if (ontologyLookupSelectionBox.getSelectedItem().toString().equals("EBI Ontology Lookup Service")) {
			if (ontoLookupProxy == null)
				ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
			if (lookupServiceWorker == null) {
				lookupServiceWorker = new OntologyLookupServiceWorker();
				lookupServiceWorker.execute();
				logger.info("Started EBI ontology lookup service.");
			} else {
				lookupServiceWorker.execute();
				logger.info("Resumed EBI ontology lookup service.");
			}
			// if (lookupChildrenServiceWorker == null) {
			// lookupChildrenServiceWorker = new
			// OntologyLookupChildrenServiceWorker();
			// lookupChildrenServiceWorker.execute();
			// }
		} else if (ontologyLookupSelectionBox.getSelectedItem().toString().equals("NCBO BioPortal Service")) {
			if (bioPortalLookupServiceWorker == null) {
				bioPortalLookupServiceWorker = new BioPortalLookupServiceWorker();
				bioPortalLookupServiceWorker.execute();
				logger.info("Started BioPortal ontology lookup service.");
			} else {
				bioPortalLookupServiceWorker.execute();
				logger.info("Resumed BioPortal ontology lookup service.");
			}
		} else {
			throw new RuntimeException("Unknown ontology lookup service requested");
		}
	}

	/**
	 * @param e
	 *            {@link PropertyChangeEvent}
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent e) {
		if ("progress" == e.getPropertyName()) { //$NON-NLS-1$
			int prog = (Integer) e.getNewValue();
			if (prog == 100 && progressDlg != null) {
				progressDlg.setVisible(false);
			}
		}
	}

	/*
	 * PRIVATE METHODS
	 */
	/**
	 * Attach all listeners
	 */
	private void attachListeners() throws Exception {
		inputPubMedQueryField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.getKeyCode() == KeyEvent.VK_ENTER) {
					onClickGenerateTerms(inputPubMedQueryField);
				}
			}
		});

		inputWebQueryField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.getKeyCode() == KeyEvent.VK_ENTER) {
					onClickGenerateTerms(inputWebQueryField);
				}
			}
		});

		inputFolderLocationField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyReleased(KeyEvent event) {
				File file = new File(inputFolderLocationField.getText());
				if (file.exists()) {
					generateTermsFromFolderButton.setEnabled(true);
				} else {
					generateTermsFromFolderButton.setEnabled(false);
				}
				if (event.getKeyCode() == KeyEvent.VK_ENTER) {
					if (file.exists()) {
						onClickGenerateTerms(inputFolderLocationField);
					} else {
						JOptionPane.showMessageDialog(guiComponent, String.format(
								Messages.getString("OntologyGenerationComponent.FileNotFound.label"), file.toString())); //$NON-NLS-1$
						JOptionPane.showMessageDialog(
								guiComponent,
								Messages.getString("OntologyGenerationComponent.") + file.toString() + Messages.getString("OntologyGenerationComponent.24")); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		});

		inputDefinitionGenerationField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.getKeyCode() == KeyEvent.VK_ENTER) {
					onClickGenerateDefinitionsManually(inputDefinitionGenerationField);
				}
			}
		});

		generateTermsFromPubMedButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickGenerateTerms(inputPubMedQueryField);
			}
		});

		generateTermsFromWebButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickGenerateTerms(inputWebQueryField);
			}
		});

		generateTermsFromTextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickGenerateTerms(inputTextArea);
			}
		});

		folderChooseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickOpenFileChooser(inputFolderLocationField);
			}
		});

		generateTermsFromFolderButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickGenerateTerms(inputFolderLocationField);
			}

		});

		generateManualDefinitionButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickGenerateDefinitionsManually(inputDefinitionGenerationField);
			}
		});

		showClipboardButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickShowClipboard();
			}
		});

		clearClipboardButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickClearClipboard();
			}
		});

		loadClipboardButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickLoadClipboard();
			}
		});

		saveClipboardButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickSaveClipboard();
			}
		});

		candidateTermsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent event) {
				if (event.getValueIsAdjusting()) {
					return;
				}
				int selectedRow = candidateTermsTable.getSelectedRow();
				if (selectedRow >= 0) {
					selectedCandidateTerm = candidateTermsTable.getModel().getTermAt(selectedRow);
					logger.trace("Selected: " + selectedCandidateTerm); //$NON-NLS-1$
					updateAllDependedOnSelectedCandidateTerm();
				}
			}
		});

		candidateTermsTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				Point p = e.getPoint();
				int column = candidateTermsTable.columnAtPoint(p);

				if (column == 2) {
					onClickGenerateDefinitions();
				} else if (column == 3) {
					if (lastSource != null) {
						onClickOpenExternalGoPubMedPage(lastSource);
					}
				}
			}
		});

		candidateTermsTable.addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent e) {
				int row = candidateTermsTable.getSelectedRow();
				// set selectedCandidateTerm
				CandidateTerm term = candidateTermsTable.getModel().getTermAt(row);
				selectedCandidateTerm = term;
				// tick row
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					boolean b = !(Boolean) candidateTermsTable.getValueAt(row, 0);
					candidateTermsTable.setValueAt(b, row, 0);
				}
			}
		});

		candidateTermsTable.getModel().addTableModelListener(new TableModelListener() {
			public void tableChanged(TableModelEvent e) {
				clipboardTabPanel.setTitleAt(0,
						Messages.getString("OntologyGenerationComponent.Clipboard") + clipboard.getSize() + ")"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		});

		onlyShowExistingTerms.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				updateTermsTableWithExistingTerms(onlyShowExistingTerms.isSelected());
			}
		});

		definitionTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				Point p = e.getPoint();
				int column = definitionTable.columnAtPoint(p);
				int row = definitionTable.rowAtPoint(p);
				// The autoscroller can generate drag events outside the Table's
				// range.
				if (column == 0) {
					updateOnClickAddDefinition(row);
				} else if (column == 1) {
					String faviconURL = definitionTable.getModel().getDefinitionAt(row).getFaviconURL();
					if (faviconURL != null) {
						openExternalWebPage(faviconURL, guiComponent);
					}
				} else if (column == 3) {
					onClickOpenDefinitionsPopup();
				}
			}
		});

		definitionTable.addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (null == selectedCandidateTerm) {
					logger.error("No term selected"); //$NON-NLS-1$
				} else {
					int row = definitionTable.getSelectedRow();
					if (KeyEvent.VK_SPACE == e.getKeyChar()) {
						boolean b = !(Boolean) definitionTable.getValueAt(row, 0);
						definitionTable.setValueAt(b, row, 0);
					}
				}
			}
		});

		ontologyTermsTable.addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent e) {
				int row = ontologyTermsTable.getSelectedRow();
				if (KeyEvent.VK_SPACE == e.getKeyChar()) {
					boolean ticked = (Boolean) ontologyTermsTable.getValueAt(row, 0);
					ontologyTermsTable.setValueAt(!ticked, row, 0);
				}
			}
		});

		ontologyTermsTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (!SwingUtilities.isLeftMouseButton(e)) {
					return;
				}
				Point p = e.getPoint();
				int column = ontologyTermsTable.columnAtPoint(p);
				int row = ontologyTermsTable.rowAtPoint(p);
				if (column == 1 && row >= 0) {
					T term = ontologyTermsTable.getModel().getTermAt(row);
					String selectedObjectID = getOntologyTermsTable().getModel().getTermId(term);
					adapter.selectOntologyTerm(selectedObjectID);
				}
			}
		});

		scrollPaneForTermsTable.getViewport().addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				updateTermsTableUsingOntologyLookup(candidateTermsTable);
			}
		});

		scrollPaneForDefinitionsTable.getViewport().addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				updateDefinitionsTableWithFavicons(definitionTable);
			}
		});

		searchTermsTextField.getTextField().addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				String text = searchTermsTextField.getTextField().getText();
				candidateTermsTable.getModel().applyFilter(text);

				candidateTermsTable.updateTermsTableStructure();
			}
		});

		searchPotentialParentsTextField.getTextField().addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				String text = searchPotentialParentsTextField.getTextField().getText();
				ontologyTermsTable.getModel().applyFilter(text);
			}
		});

		inputDefinitionGenerationField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				if (selectedCandidateTerm != null) {
					if (inputDefinitionGenerationField.getText().equals("")) { //$NON-NLS-1$
						generateManualDefinitionButton.setEnabled(false);
					} else {
						generateManualDefinitionButton.setEnabled(true);
					}
				}
			}
		});

		searchDefTextField.getTextField().addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				String text = searchDefTextField.getTextField().getText();
				definitionTable.getModel().applyFilter(text);
			}
		});

		inputPubMedQueryField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				generateTermsFromPubMedButton.setFocusable(true);
				generateTermsFromPubMedButton.setEnabled(true);
			}
		});

		inputWebQueryField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				generateTermsFromWebButton.setFocusable(true);
				generateTermsFromWebButton.setEnabled(true);
			}
		});

		inputTextArea.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				generateTermsFromTextButton.setFocusable(true);
				generateTermsFromTextButton.setEnabled(true);
			}
		});

		inputDefinitionGenerationField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent evt) {
				generateManualDefinitionButton.setFocusable(true);
				generateManualDefinitionButton.setEnabled(true);
			}
		});

		editDefArea.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				final String currentDefinition = editDefArea.getText().trim();
				if (selectedCandidateTerm != null) {
					final String userDefinedDefinition = selectedCandidateTerm.getUserDefinedDefinition();
					String definitionForExistingTerm = null;
					if (selectedCandidateTerm.isInLoadedOntology()) {
						definitionForExistingTerm = selectedCandidateTerm.getExistingOntologyClass().getDefinition(
								adapter.getLanguage());
					}

					if (userDefinedDefinition == null && definitionForExistingTerm == null
							&& currentDefinition.length() == 0) {
						updateSaveDefWarningLabel(false);
					} else if (userDefinedDefinition != null && currentDefinition.equals(userDefinedDefinition.trim())) {
						updateSaveDefWarningLabel(false);
					} else if (definitionForExistingTerm != null
							&& currentDefinition.equals(definitionForExistingTerm.trim())) {
						updateSaveDefWarningLabel(false);
					} else {
						updateSaveDefWarningLabel(true);
					}
				}
			}
		});

		deleteDefButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickDeleteDefinition();
			}
		});

		saveDefButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickSaveDefinition();
			}
		});

		editNameTextField.addCaretListener(new CaretListener() {

			public void caretUpdate(CaretEvent e) {
				if (selectedCandidateTerm != null) {
					String currentLabel = editNameTextField.getText().trim();
					if (currentLabel.equals(selectedCandidateTerm.getUserDefinedLabel())
							|| currentLabel.equals(selectedCandidateTerm.getLabel())) {
						updateSaveLabelWarningLabel(false);
					} else {
						updateSaveLabelWarningLabel(true);
					}
				}
			}
		});

		DocumentListener proxyDocumentListener = new DocumentListener() {
			public void changedUpdate(DocumentEvent event) {
				onChangeProxyTextField();
			}

			public void insertUpdate(DocumentEvent event) {
				onChangeProxyTextField();
			}

			public void removeUpdate(DocumentEvent event) {
				onChangeProxyTextField();
			}
		};

		proxyHostTextField.getDocument().addDocumentListener(proxyDocumentListener);
		proxyPortTextField.getDocument().addDocumentListener(proxyDocumentListener);
		proxyUserTextField.getDocument().addDocumentListener(proxyDocumentListener);
		proxyPasswordField.getDocument().addDocumentListener(proxyDocumentListener);

		proxyResetButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickProxyResetButton();
			}
		});

		proxySaveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickProxySaveButton();
			}
		});

		saveLabelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickSaveLabel();
			}
		});

		ontologyTermsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (e.getValueIsAdjusting()) {
					return;
				}
				int selectedRow = ontologyTermsTable.getSelectedRow();
				if (selectedRow != selectedParentTermRow && selectedRow >= 0) {
					selectedParentTermRow = selectedRow;
					Set<String> tickedTerms = new HashSet<String>();
					tickedTerms.addAll(ontologyTermsTable.getModel().getTickedTerms());

					T term = ontologyTermsTable.getModel().getTermAt(selectedRow);
					String selectedObjectID = getOntologyTermsTable().getModel().getTermId(term);

					ontologyTermsTable.setRowSelectionInterval(
							ontologyTermsTable.getModel().getRowFromTerm(selectedObjectID), ontologyTermsTable
									.getModel().getRowFromTerm(selectedObjectID));
					ontologyTermsTable.getModel().setTickedTerms(tickedTerms);
				}
			}
		});

		addToOntologyButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onClickAddToOntology();
			}
		});

		checkboxShowTickedParents.addItemListener(new ItemListener() {

			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					ontologyTermsTable.getModel().setShowOnlyTicked(true);
				} else {
					ontologyTermsTable.getModel().setShowOnlyTicked(false);
				}
			}
		});

		languageSelectionBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JComboBox comboBox = (JComboBox) e.getSource();
				String selectedItem = (String) comboBox.getSelectedItem();
				adapter.setLocale(ontologyLocales.get(selectedItem));
			}
		});

		ontologyLookupSelectionBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				initOntologyLookupServices();
			}
		});

		// SIBLING GENERATION

		DocumentListener seedTermDocumentListener = new DocumentListener() {
			public void changedUpdate(DocumentEvent arg0) {
				updateGenerateSiblingsButton();
			}

			public void insertUpdate(DocumentEvent arg0) {
				updateGenerateSiblingsButton();
			}

			public void removeUpdate(DocumentEvent arg0) {
				updateGenerateSiblingsButton();
			}
		};

		KeyAdapter seedTermKeyAdapter = new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent event) {
				if (event.getKeyCode() == KeyEvent.VK_ENTER && !inputSeedTermField1.getText().equals("")
						&& !inputSeedTermField2.getText().equals("")) {
					onClickGenerateSiblings();
				}
			}
		};

		inputSeedTermField1.getDocument().addDocumentListener(seedTermDocumentListener);
		inputSeedTermField2.getDocument().addDocumentListener(seedTermDocumentListener);
		inputSeedTermField3.getDocument().addDocumentListener(seedTermDocumentListener);

		inputSeedTermField2.addKeyListener(seedTermKeyAdapter);
		inputSeedTermField3.addKeyListener(seedTermKeyAdapter);

		generateSiblingsButton.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				onClickGenerateSiblings();
			}
		});

		ProxyInfo.registerListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				proxyHostTextField.setText(ProxyInfo.getHost() != null ? ProxyInfo.getHost() : ""); //$NON-NLS-1$
				proxyPortTextField.setText(ProxyInfo.getPort() != null ? ProxyInfo.getPort() : ""); //$NON-NLS-1$
				proxyUserTextField.setText(ProxyInfo.getUsername() != null ? ProxyInfo.getUsername() : ""); //$NON-NLS-1$
				proxyPasswordField.setText(ProxyInfo.getPassword() != null ? ProxyInfo.getPassword() : ""); //$NON-NLS-1$
			}
		});

	}

	/**
	 * Open a fileChooser to get a location for pdf files
	 */
	private void onClickOpenFileChooser(JTextComponent textComponent) {
		if (textComponent == null) {
			return;
		}

		JFileChooser fileChooser = new JFileChooser();

		String sfolder = textComponent.getText();
		logger.trace("sFolder: " + sfolder); //$NON-NLS-1$
		if (!sfolder.trim().equals("")) { //$NON-NLS-1$
			File file = new File(sfolder);
			if (file.isDirectory()) {
				fileChooser.setCurrentDirectory(file.getAbsoluteFile());
			} else if (file.isFile()) {
				fileChooser.setCurrentDirectory(file.getParentFile());
			}
		}

		class pdfFileFilter extends FileFilter {

			@Override
			public boolean accept(File file) {
				String filename = file.getName();
				if (file.isDirectory()) {
					return true;
				}
				return filename.endsWith(".pdf"); //$NON-NLS-1$
			}

			@Override
			public String getDescription() {
				return "*.pdf"; //$NON-NLS-1$
			}
		}

		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addChoosableFileFilter(new pdfFileFilter());

		if (fileChooser.showOpenDialog(new JFrame()) == JFileChooser.APPROVE_OPTION) {
			sfolder = fileChooser.getSelectedFile().getAbsolutePath();
			textComponent.setText(sfolder);

			generateTermsFromFolderButton.setEnabled(true);
			generateTermsFromFolderButton.setFocusable(true);
		} else {
			generateTermsFromFolderButton.setEnabled(false);
			generateTermsFromFolderButton.setFocusable(false);
		}
	}

	/**
	 * Shows the clip board in termsTable
	 */
	private void onClickShowClipboard() {
		// clear first
		clearAllDependendOnSelectedTerm();
		searchTermsTextField.getTextField().setText(null);
		updateClipboard(candidateTermsTable);
		// updateUI();
		updateTermsTableUsingOntologyLookup(candidateTermsTable);
	}

	/**
	 * Populate {@link TermsTable} with terms from clip board
	 * 
	 * @param table
	 */
	private void updateClipboard(TermsTable table) {
		ArrayList<CandidateTerm> list = new ArrayList<CandidateTerm>(clipboard.getAllCandidateTerms());
		Collections.sort(list, new Comparator<CandidateTerm>() {
			public int compare(CandidateTerm o1, CandidateTerm o2) {
				int compare = Double.compare(o1.getScore(), o2.getScore());
				if (compare != 0) {
					return compare;
				}
				return o1.getGeneratedLabel().compareTo(o2.getGeneratedLabel());
			}
		});
		table.setTerms(list);
		table.getColumnModel().getColumn(1)
				.setHeaderValue(Messages.getString("OntologyGenerationComponent.ShowingClipboardColumnHeader")); //$NON-NLS-1$
		// Scroll and select
		JTableHelper.scrollToTopAndSelectFirst(table);
	}

	/**
	 * Clears the clip board
	 */
	private void onClickClearClipboard() {
		candidateTermsTable.getModel().unTickAll();
		clipboard.clear();

		// clear
		clearAllDependendOnSelectedTerm();
		clipboardTabPanel
				.setTitleAt(
						0,
						Messages.getString("OntologyGenerationComponent.ClipboardTabPanelTitle") + "(" + clipboard.getSize() + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		guiComponent.updateUI();
	}

	/**
	 * Save clip board to file
	 */
	private void onClickSaveClipboard() {

		JFileChooser chooser = new JFileChooser();
		int option = chooser.showSaveDialog(guiComponent);
		if (option == JFileChooser.APPROVE_OPTION) {
			// statusbar.setText("You saved " +
			// ((chooser.getSelectedFile()!=null)?
			// chooser.getSelectedFile().getName():"nothing"));
		} else {
			// statusbar.setText("You canceled.");
		}
		File fileToSave = chooser.getSelectedFile();
		if (null == fileToSave) {
			return;
		}

		// generate content to save
		Collection<CandidateTerm> allTermsFromClipboard = clipboard.getAllCandidateTerms();
		StringBuffer outBuffer = new StringBuffer();
		for (CandidateTerm candidateTerm : allTermsFromClipboard) {
			if (candidateTerm.isTicked()) {
				outBuffer.append(candidateTerm.getGeneratedLabel().replace("\t", " ")); //$NON-NLS-1$ //$NON-NLS-2$
				outBuffer.append("\t"); //$NON-NLS-1$
				if (null != candidateTerm.getAbbreviations()) {
					Iterator<String> iterator = candidateTerm.getAbbreviations().iterator();
					outBuffer.append("["); //$NON-NLS-1$
					while (iterator.hasNext()) {
						outBuffer.append(iterator.next().replace("\t", " ").replace("|", ";")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
						if (iterator.hasNext()) {
							outBuffer.append("|"); //$NON-NLS-1$
						}
					}

				}
				outBuffer.append("]"); //$NON-NLS-1$
				outBuffer.append("\t"); //$NON-NLS-1$
				if (null != candidateTerm.getLexicalRepresentations()) {
					Iterator<String> iterator = candidateTerm.getLexicalRepresentations().iterator();
					outBuffer.append("["); //$NON-NLS-1$
					while (iterator.hasNext()) {
						outBuffer.append(iterator.next().replace("\t", " ").replace("|", ";")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
						if (iterator.hasNext()) {
							outBuffer.append("|"); //$NON-NLS-1$
						}
					}
				}
				outBuffer.append("]"); //$NON-NLS-1$
				outBuffer.append("\t"); //$NON-NLS-1$
				if (null != candidateTerm.getUserDefinedLabel()) {
					outBuffer.append(candidateTerm.getUserDefinedLabel().replace("\t", " ")); //$NON-NLS-1$ //$NON-NLS-2$
				}
				outBuffer.append("\t"); //$NON-NLS-1$
				if (null != candidateTerm.getUserDefinedDefinition()) {
					outBuffer.append(candidateTerm.getUserDefinedDefinition().replace("\t", " ")); //$NON-NLS-1$ //$NON-NLS-2$
				}
				outBuffer.append("\n"); //$NON-NLS-1$
			}
		}

		BufferedWriter bufferedWriter = null;
		try {
			bufferedWriter = new BufferedWriter(new FileWriter(fileToSave));
			bufferedWriter.write(outBuffer.toString());
		} catch (IOException exception) {
			logger.error("Saving terms in clipboard failed!", exception); //$NON-NLS-1$
		} finally {
			if (bufferedWriter != null) {
				try {
					bufferedWriter.close();
				} catch (IOException exception) {
					logger.error("Saving terms in clipboard failed!", exception); //$NON-NLS-1$
				}
			}
		}
	}

	/**
	 * Load clip board from file
	 */
	private void onClickLoadClipboard() {
		JFileChooser chooser = new JFileChooser();
		chooser.setMultiSelectionEnabled(true);
		int option = chooser.showOpenDialog(guiComponent);
		if (option == JFileChooser.APPROVE_OPTION) {
			File[] sf = chooser.getSelectedFiles();
			String filelist = "nothing"; //$NON-NLS-1$
			if (sf.length > 0)
				filelist = sf[0].getName();
			for (int i = 1; i < sf.length; i++) {
				filelist += ", " + sf[i].getName(); //$NON-NLS-1$
			}
		}
		File fileToLoad = chooser.getSelectedFile();
		if (null == fileToLoad) {
			return;
		}

		// read file
		List<String> lines = new ArrayList<String>();
		BufferedReader bufferedReader = null;
		try {
			bufferedReader = new BufferedReader(new FileReader(fileToLoad));
			while (bufferedReader.ready()) {
				String readLine = bufferedReader.readLine();
				lines.add(readLine);
			}
		} catch (FileNotFoundException exception) {
			logger.error("Loading terms in clipboard failed!", exception); //$NON-NLS-1$
		} catch (IOException exception) {
			logger.error("Loading terms in clipboard failed!", exception); //$NON-NLS-1$
		} finally {
			if (bufferedReader != null) {
				try {
					bufferedReader.close();
				} catch (IOException exception) {
					logger.error("Loading terms in clipboard failed!", exception); //$NON-NLS-1$
				}
			}
		}
		for (String line : lines) {
			String[] split = line.split("\t"); //$NON-NLS-1$
			if (split.length > 0) {
				CandidateTerm candidateTerm = new CandidateTerm();
				candidateTerm.setTicked(true);
				candidateTerm.setVisible(true);
				candidateTerm.setGeneratedLabel(split[0]);
				candidateTerm.addType(CandidateTerm.TYPE_LOADED);
				if (split.length > 1) {
					String abbreviationString = split[1];
					abbreviationString = abbreviationString.subSequence(1, abbreviationString.length() - 1).toString();
					String[] abbreviations = abbreviationString.split("\\|"); //$NON-NLS-1$
					for (String abbrev : abbreviations) {
						candidateTerm.addAbbreviation(abbrev);
					}
				}
				if (split.length > 2) {
					String lexicalString = split[2];
					lexicalString = lexicalString.subSequence(1, lexicalString.length() - 1).toString();
					String[] lexicalRepresentations = lexicalString.split("\\|"); //$NON-NLS-1$
					for (String lex : lexicalRepresentations) {
						candidateTerm.addLexicalRepresentation(lex);
					}
				}
				if (split.length > 3) {
					if (split[3].length() > 0) {
						candidateTerm.setUserDefinedLabel(split[3]);
					}
				}
				if (split.length > 4) {
					if (split[4].length() > 0) {
						candidateTerm.setUserDefinedDefinition(split[4]);
					}
				}
				candidateTermsTable.getModel().addTermToClipboard(candidateTerm);
			}
		}
		onClickShowClipboard();
	}

	/**
	 * Invokes TermGenerationWorker Thread on "Generate Terms" Button
	 * 
	 * @param inputPubMedQueryField2
	 */
	private void onClickGenerateTerms(JTextComponent textComponent) {
		showProgressDlg(false);
		String inputData = textComponent.getText().trim();
		// call to term generation thread for fetching terms from web service

		ontologyLookupQueue.clear();
		ontologyLookupChildrenQueue.clear();

		String source = null;
		if (textComponent.equals(inputPubMedQueryField)) {
			source = SOURCE_PUBMED;
		} else if (textComponent.equals(inputWebQueryField)) {
			source = SOURCE_WEB;
		} else if (textComponent.equals(inputTextArea)) {
			source = SOURCE_TEXT;
		} else if (textComponent.equals(inputFolderLocationField)) {
			source = SOURCE_FOLDER;
		}
		lastSource = source;

		searchTermsTextField.getTextField().setText(null);

		TermGenerationServiceWorker worker = new TermGenerationServiceWorker(inputData, candidateTermsTable, source);
		worker.execute();
		worker.addPropertyChangeListener(this);

		showProgressDlg(true);
	}

	private void onClickGenerateSiblings() {
		showProgressDlg(false);

		ontologyLookupQueue.clear();
		ontologyLookupChildrenQueue.clear();

		boolean useOntologySeedTerms = true;
		if (tentativeSeedTerms.size() > 0) {
			useOntologySeedTerms &= (inputSeedTermField1.getText().equals(tentativeSeedTerms.get(0).getLabel(
					adapter.getLanguage())));
			useOntologySeedTerms &= (inputSeedTermField2.getText().equals(tentativeSeedTerms.get(1).getLabel(
					adapter.getLanguage())));
			if (inputSeedTermField3.getText().length() > 0) {
				useOntologySeedTerms &= (inputSeedTermField3.getText().equals(tentativeSeedTerms.get(2).getLabel(
						adapter.getLanguage())));
			}
		} else {
			useOntologySeedTerms = false;
		}

		SiblingGenerationServiceWorker worker = null;

		if (useOntologySeedTerms) {
			worker = new SiblingGenerationServiceWorker(tentativeSeedTerms,
					selectedCandidateTerm.getExistingOntologyClass(), candidateTermsTable);
		} else {
			List<String> seedTermStrings = new ArrayList<String>();
			if (!inputSeedTermField1.getText().equals(""))
				seedTermStrings.add(inputSeedTermField1.getText());
			if (!inputSeedTermField2.getText().equals(""))
				seedTermStrings.add(inputSeedTermField2.getText());
			if (!inputSeedTermField3.getText().equals(""))
				seedTermStrings.add(inputSeedTermField3.getText());

			worker = new SiblingGenerationServiceWorker(seedTermStrings, candidateTermsTable);
		}

		worker.execute();
		worker.addPropertyChangeListener(this);

		showProgressDlg(true);
	}

	private void updateTermsTableHeader(String source, String inputData) {
		if (source.equals(SOURCE_PUBMED)) {
			String message = String.format(
					Messages.getString("OntologyGenerationComponent.PubMedTableHeader.label"), inputData); //$NON-NLS-1$
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		} else if (source.equals(SOURCE_WEB)) {
			String message = String.format(
					Messages.getString("OntologyGenerationComponent.WebTableHeader.label"), inputData); //$NON-NLS-1$
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		} else if (source.equals(SOURCE_SIBLING)) {
			String message = Messages.getString("OntologyGenerationComponent.SiblingTableHeader.label"); //$NON-NLS-1$
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		} else if (source.equals(SOURCE_TEXT)) {
			String message = String.format(
					Messages.getString("OntologyGenerationComponent.TextTableHeader.label"), inputData.length() / 1024); //$NON-NLS-1$
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		} else if (source.equals(SOURCE_FOLDER)) {
			String message = String.format(
					Messages.getString("OntologyGenerationComponent.FolderTableHeader.label"), inputData); //$NON-NLS-1$
			candidateTermsTable.getColumnModel().getColumn(1).setHeaderValue(message);
			logger.debug(message);
		}
		candidateTermsTable.getTableHeader().repaint();

		searchTermsTextField.getTextField().setText(null);

	}

	/**
	 * Launches web browser to get results from www.gopubmed.org for queryTerm
	 * and termsTable selected term.
	 */
	private void onClickOpenExternalGoPubMedPage(String pLastSource) {
		String goURL;
		JTextField textField;
		if (pLastSource == SOURCE_PUBMED) {
			textField = inputPubMedQueryField;
			goURL = "http://www.gopubmed.org/search?q="; //$NON-NLS-1$
		} else if (pLastSource == SOURCE_WEB) {
			textField = inputWebQueryField;
			goURL = "http://www.gopubmed.org/web/goweb/search?q="; //$NON-NLS-1$
		} else {
			return;
		}

		String encodedGeneratedTerm;
		String encodedqServiceTerm;

		int colNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedColumn();
		int rowNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedRow();

		if (colNumber == 3 && rowNumber >= 0) {
			CandidateTerm candidateTerm = candidateTermsTable.getModel().getTermAt(rowNumber);
			String generatedTerm = putInQuotes(candidateTerm.getGeneratedLabel());
			String qServiceTerm = putInQuotes(textField.getText());

			try {
				encodedGeneratedTerm = URLEncoder.encode(generatedTerm, "UTF-8"); //$NON-NLS-1$
				encodedqServiceTerm = URLEncoder.encode(qServiceTerm, "UTF-8"); //$NON-NLS-1$
			} catch (UnsupportedEncodingException exception) {
				throw new RuntimeException(exception);
			}

			StringBuffer qBuffer = new StringBuffer();
			qBuffer.append(goURL);
			qBuffer.append(encodedGeneratedTerm);
			qBuffer.append("+AND+"); //$NON-NLS-1$
			qBuffer.append(encodedqServiceTerm);
			qBuffer.append("&t=protege"); //$NON-NLS-1$

			String url = qBuffer.toString();

			openExternalWebPage(url, guiComponent);
		}
	}

	@SuppressWarnings({ "rawtypes" })
	private static void openExternalWebPage(String url, Component guiComponent) {
		String errMsg = Messages.getString("OntologyGenerationComponent.ErrorAttemptingToLaunchWebBrowser"); //$NON-NLS-1$

		String osName = System.getProperty("os.name"); //$NON-NLS-1$
		try {
			if (osName.startsWith("Mac OS")) { //$NON-NLS-1$
				Class fileMgr = Class.forName("com.apple.eio.FileManager"); //$NON-NLS-1$
				Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] { String.class }); //$NON-NLS-1$
				openURL.invoke(null, new Object[] { url });
			} else if (osName.startsWith("Windows")) //$NON-NLS-1$
				Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url); //$NON-NLS-1$
			else { // assume Unix or Linux
				String[] browsers = { "firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
				String browser = null;
				for (int count = 0; count < browsers.length && browser == null; count++) {
					if (Runtime.getRuntime().exec(new String[] { "which", browsers[count] }).waitFor() == 0) { //$NON-NLS-1$
						browser = browsers[count];
					}
				}
				if (browser == null) {
					logger.error("Could not find web browser"); //$NON-NLS-1$
				} else {
					Runtime.getRuntime().exec(new String[] { browser, url });
				}
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(guiComponent, errMsg + ":\n" + e.getLocalizedMessage()); //$NON-NLS-1$
		}

	}

	/**
	 * Instantiates DefinitionWorker thread responsible to call
	 * definitionGeneration Service
	 */
	private void onClickGenerateDefinitions() {
		int rowNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedRow();
		int colNumber = OntologyGenerationComponent.this.candidateTermsTable.getSelectedColumn();

		// For each definition query first clear the termsInDefComboBox
		if (colNumber == 2 && rowNumber >= 0) {
			// find DEFINITIONS
			// call to definition web service to fetch definitions
			String label = selectedCandidateTerm.getLabel();
			logger.debug(String.format(
					Messages.getString("OntologyGenerationComponent.GenerateDefinitionsForTerm"), label)); //$NON-NLS-1$
			startGenerateDefinitions(label);
		}
	}

	private void startGenerateDefinitions(String label) {
		showProgressDlg(false);

		DefinitionGenerationServiceWorker defworker = new DefinitionGenerationServiceWorker(label, null,
				definitionTable);
		defworker.addPropertyChangeListener(this);
		defworker.execute();
		showProgressDlg(true);
	}

	/**
	 * generated a new CandidateDefinition object
	 * 
	 * @param definitionField
	 */
	/*
	 * SPEC: generate definition for the term, if the term is visible, move term
	 * to the top, if not make it visible and move it to the top and select
	 * (selection should make the definition, if exist, listed first)
	 */
	private void onClickGenerateDefinitionsManually(JTextField definitionField) {
		String inputLabel = definitionField.getText().trim();
		selectOrCreateCandidateTerm(inputLabel);
		startGenerateDefinitions(inputLabel);
	}

	private void selectOrCreateCandidateTerm(String inputLabel) {
		CandidateTerm term;

		if (candidateTermCache.hasCandidateTermWithLabel(inputLabel)) {
			// case that the term was created for caching before (e.g. when
			// selecting a term in external editor), reuse this term
			term = candidateTermCache.get(inputLabel);
		} else if (clipboard.hasCandidateTermWithLabel(inputLabel)) {
			// case that the term is in clipboard
			term = clipboard.get(inputLabel);
		} else {
			// create new term if no term exists with this label
			CandidateTerm newTerm = new CandidateTerm(inputLabel, new HashSet<String>(), null, 1,
					CandidateTerm.TYPE_MANUAL);
			newTerm.setUserDefinedLabel(inputLabel);
			newTerm.addLexicalRepresentation(inputLabel);
			term = newTerm;
			candidateTermCache.addTerm(term);
		}
		List<CandidateTerm> allTerms = candidateTermsTable.getModel().getAllTerms();
		for (CandidateTerm candidateTerm : allTerms) {
			if (candidateTerm.getLabel().equals(term.getLabel())) {
				term = candidateTerm;
			} else if (candidateTerm.getLexicalRepresentations().contains(term.getLabel())) {
				term = candidateTerm;
			}
		}
		// if not selected or selected but not in the table yet (e.g was
		// selected in external editor),
		// put term for which definitions are generated to top of the termsTable
		if (!term.equals(selectedCandidateTerm) || !allTerms.contains(selectedCandidateTerm)) {
			List<CandidateTerm> newTermsList = new ArrayList<CandidateTerm>(allTerms.size() + 1);
			newTermsList.add(term);
			for (CandidateTerm candidateTerm : allTerms) {
				if (!candidateTerm.equals(term)) {
					newTermsList.add(candidateTerm);
				}
			}
			candidateTermsTable.setTerms(newTermsList);

			// scroll top and select first
			if (onlyShowExistingTerms.isSelected() && !term.isInLoadedOntology()) {
				onlyShowExistingTerms.setSelected(false);
				updateTermsTableWithExistingTerms(false);
			} else {
				JTableHelper.scrollToTopAndSelectFirst(candidateTermsTable);
			}
		}
		// trigger update ontology lookup in terms table
		updateTermsTableUsingOntologyLookup(candidateTermsTable);
	}

	/**
	 * Picks the clicked definition, sets it ticked and updates depended GUI
	 * components
	 * 
	 * @param rowIndex
	 */
	private void updateOnClickAddDefinition(int rowIndex) {
		logger.trace("onClickSelectDefinition " + rowIndex); //$NON-NLS-1$
		if (null == selectedCandidateTerm) {
			logger.error("No term selected"); //$NON-NLS-1$
			return;
		}

		CandidateDefinition definition = definitionTable.getModel().getDefinitionAt(rowIndex);
		definition.setTicked();

		updateEditDefAreaString(definition.getDefinition());
	}

	/**
	 * Opens a popup showing the URLs for the definition and its similar
	 * definitions.
	 */
	private void onClickOpenDefinitionsPopup() {
		int rowNumber = OntologyGenerationComponent.this.definitionTable.getSelectedRow();

		DefinitionsPopup<T, R> definitionsPopup = new DefinitionsPopup<T, R>(guiComponent, this);
		definitionsPopup.initPopup(definitionTable.getModel().getDefinitionAt(rowNumber));
		definitionsPopup.setVisible(true);
	}

	private void updateSaveDefWarningLabel(boolean enableWarning) {
		if (enableWarning) {
			saveDefWarningLabel.setText(Messages.getString("OntologyGenerationComponent.SaveDefinitionWarning.label")); //$NON-NLS-1$
		} else {
			saveDefWarningLabel.setText(""); //$NON-NLS-1$
		}
	}

	/**
	 * Save definition for {@link CandidateTerm} and update add-to-ontology
	 * table
	 */
	private void onClickSaveDefinition() {
		if (null == selectedCandidateTerm) {
			logger.error("No term selected"); //$NON-NLS-1$
		} else {
			String defText = editDefArea.getText();
			selectedCandidateTerm.setUserDefinedDefinition(defText);
			candidateTermCache.addTerm(selectedCandidateTerm);
			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable,
					definitionTable);
			// commit to OBOClass if term is known
			adapter.commitDefinition(selectedCandidateTerm);
			updateSaveDefWarningLabel(false);
		}
	}

	/**
	 * Delete definition for {@link CandidateTerm} and update add-to-ontology
	 * table
	 */
	private void onClickDeleteDefinition() {
		if (null == selectedCandidateTerm) {
			logger.error("No term selected"); //$NON-NLS-1$
		} else {
			selectedCandidateTerm.setUserDefinedDefinition(null);
			selectedCandidateTerm.setGeneratedDefinitions(null);
			candidateTermCache.addTerm(selectedCandidateTerm);
			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable,
					definitionTable);
			// commit to OBOClass if term is known
			adapter.commitDefinition(selectedCandidateTerm);
			updateSaveDefWarningLabel(false);
			deleteDefButton.setEnabled(false);
		}
	}

	private void updateSaveLabelWarningLabel(boolean enableWarning) {
		if (enableWarning && selectedCandidateTerm != null) {
			saveLabelWarningLabel.setText(Messages.getString("OntologyGenerationComponent.LabelNotSaveWarning.label")); //$NON-NLS-1$
		} else {
			saveLabelWarningLabel.setText(""); //$NON-NLS-1$
		}
	}

	/**
	 * Save definition for {@link CandidateTerm} and update add-to-ontology
	 * table
	 */
	private void onClickSaveLabel() {
		String inputLabel = editNameTextField.getText();
		if (null == selectedCandidateTerm) {
			logger.warn("No term selected"); //$NON-NLS-1$
			selectOrCreateCandidateTerm(inputLabel);
		}
		selectedCandidateTerm.setUserDefinedLabel(inputLabel);
		ontologyLookupQueue.offer(selectedCandidateTerm);

		// candidateTermCache.addTerm(selectedCandidateTerm);
		// commit to OBOClass if term is known
		adapter.commitLabel(selectedCandidateTerm);
		updateSaveLabelWarningLabel(false);
	}

	/**
	 * Add selected terms to application ontology model
	 */
	private void onClickAddToOntology() {
		boolean includeChildren = checkboxIncludeChildren.isSelected();
		boolean includeBranch = false; // checkboxIncludeBranch.isSelected();

		ArrayList<ParentRelationEntry<R>> parentRelationList = new ArrayList<ParentRelationEntry<R>>(10);
		for (String parentId : ontologyTermsTable.getModel().getTickedTerms()) {
			R relationType = ontologyTermsTable.getModel().getRelationType(parentId);
			ParentRelationEntry<R> parentRelationEntry = new ParentRelationEntry<R>(parentId, relationType);
			parentRelationList.add(parentRelationEntry);
		}
		adapter.commitAddToOntologyAsChildOfOntologyTerm(selectedCandidateTerm, parentRelationList, includeChildren,
				includeBranch);
	}

	private void onChangeProxyTextField() {
		proxySaveButton.setEnabled(true);
		proxyResetButton
				.setEnabled((proxyHostTextField.getDocument().getLength() > 0
						|| proxyPortTextField.getDocument().getLength() > 0
						|| proxyUserTextField.getDocument().getLength() > 0 || proxyPasswordField.getDocument()
						.getLength() > 0));
	}

	/**
	 * Reset all proxy settings and use JVM or System settings
	 */
	private void onClickProxyResetButton() {
		ProxyInfo.resetToSystemProxySettings();

		proxyResetButton.setEnabled(false);
	}

	private void onClickProxySaveButton() {
		ProxyInfo.setHost(proxyHostTextField.getText());
		ProxyInfo.setPort(proxyPortTextField.getText());
		ProxyInfo.setUsername(proxyUserTextField.getText());
		ProxyInfo.setPassword(new String(proxyPasswordField.getPassword()));

		proxySaveButton.setEnabled(false);
	}

	/**
	 * Recursively get all the descendants
	 * 
	 * @param children
	 */
	private void lookedUpChildTerm(List<String> children) {
		try {
			HashMap<String, String> idToName = new HashMap<String, String>();

			for (String childId : children) {
				OBOLookupResult lookupResult = ontoLookupProxy.getChildren(childId, 1);
				if (lookupResult.getTerms() != null) {
					for (OBOLookupTerm lookupTerm : lookupResult.getTerms()) {
						idToName.put(lookupTerm.getOboID(), lookupTerm.getLabel());
					}
				}
				List<String> children2 = new ArrayList<String>();
				if (lookupResult.getRelations() != null) {
					for (OBOLookupRelation relation : lookupResult.getRelations()) {
						String parent = idToName.get(relation.getOboTermId());
						String child = idToName.get(relation.getOboChildTermID());
						children2.add(relation.getOboChildTermID());
						String relationString = relation.getOboRelationShipType();
						logger.error(String.format("%s -%s-> %s", parent, relationString, child)); //$NON-NLS-1$
					}
				}
				lookedUpChildTerm(children2);
			}

		} catch (RemoteException exception) {
			logger.error("Lookup for known children via ontology lookup service failed."); //$NON-NLS-1$
		}

	}

	/**
	 * Displays term selected in the termsTable and updates all depending gui
	 * components
	 */
	public void updateAllDependedOnSelectedCandidateTerm() {
		logger.trace("UPDATE updatedAllDependedOnSelectedTerm() for :" + selectedCandidateTerm); //$NON-NLS-1$
		clearAllDependendOnSelectedTerm();

		if (null == selectedCandidateTerm) {
			logger.warn("Invalid selection in termsTable, term is assumed to be selected"); //$NON-NLS-1$
		} else {
			List<CandidateDefinition> generatedDefinitions = selectedCandidateTerm.getGeneratedDefinitions();

			inputDefinitionGenerationField.setText(selectedCandidateTerm.getLabel());
			if (null != generatedDefinitions) {
				definitionTable.removeAll();
				definitionTable.setDefinitions(generatedDefinitions);

			}
			updateEditDefArea(null);

			if (selectedCandidateTerm.getGeneratedDefinitions() != null
					&& !selectedCandidateTerm.getGeneratedDefinitions().isEmpty()) {
				definitionTable
						.getColumnModel()
						.getColumn(2)
						.setHeaderValue(
								Messages.getString("OntologyGenerationComponent.DefinitionsForTableHeader") + selectedCandidateTerm.getLabel() + "\""); //$NON-NLS-1$ //$NON-NLS-2$
				definitionTable.getTableHeader().repaint();
			}

			if (!editNameTextField.getText().equals(selectedCandidateTerm.getLabel())) {
				editNameTextField.setText(selectedCandidateTerm.getLabel());
				editNameTextField.updateUI();
			}

			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable,
					definitionTable);
			// updateSynonymOrChildTable();
			updateParentAsSimiliarTerm();
			updateParentAsExistingTerm();

			logger.trace("SelectedCandidateTerm Name: " + selectedCandidateTerm.getLabel()); //$NON-NLS-1$
		}
	}

	/**
	 * Add similar (based on substring inclusion) terms to similiarTermsComboBox
	 * based on substring comparison
	 */
	private void updateParentAsSimiliarTerm() {
		logger.trace("UPDATE SIMILAR TERMS for :" + selectedCandidateTerm.getLabel()); //$NON-NLS-1$

		Collection<String> labels = new HashSet<String>();
		labels.addAll(selectedCandidateTerm.getLexicalRepresentations());
		labels.addAll(selectedCandidateTerm.getAbbreviations());
		if (null != selectedCandidateTerm.getLabel())
			labels.add(selectedCandidateTerm.getLabel());
		if (null != selectedCandidateTerm.getUserDefinedLabel())
			labels.add(selectedCandidateTerm.getUserDefinedLabel());
		if (null != selectedCandidateTerm.getGeneratedLabel())
			labels.add(selectedCandidateTerm.getGeneratedLabel());

		List<String> idsOfSimilarTerms = adapter.lookupOntologyTermIdsFromIndexFuzzy(labels);
		ontologyTermsTable.getModel().updateSimilarTerms(labels, idsOfSimilarTerms);
	}

	/**
	 * Updated the parent terms in the existing loaded ontology model
	 */
	private void updateParentAsExistingTerm() {
		if (selectedCandidateTerm != null) {
			OntologyClassInterface ontologyClass = adapter.getOntologyClassForCandidateTerm(selectedCandidateTerm);
			if (ontologyClass != null) {
				Map<String, String> parents = ontologyClass.getParentMap(adapter.getLanguage());
				// clear and set new parents
				ontologyTermsTable.getModel().setParentsTermsOfExistingCandidateTerm(parents);
			}
		}
	}

	/**
	 * Update the table holding synonyms and children of the currently selected
	 * terms
	 */
	/*
	 * private void updateSynonymOrChildTable() { // update synonymous terms
	 * List<CandidateTerm> synonymousOrChildTerms = new
	 * ArrayList<CandidateTerm>(); Iterable<String> userDefinedAbbreviations =
	 * selectedCandidateTerm.getAbbreviations(); for (String string :
	 * userDefinedAbbreviations) { CandidateTerm abbreviationTerm = new
	 * CandidateTerm(); abbreviationTerm.setGeneratedLabel(string);
	 * abbreviationTerm.addLexicalRepresentation(string);
	 * abbreviationTerm.addType(CandidateTerm.TYPE_ABBREVIATION);
	 * synonymousOrChildTerms.add(abbreviationTerm);
	 * abbreviationTerm.setVisible(true); } // update children if
	 * (selectedCandidateTerm.getExistingChildTerms() == null) { // no terms
	 * looked up yet, add to queue if
	 * (!ontologyLookupChildrenQueue.contains(selectedCandidateTerm)) {
	 * logger.info("ADD to queue: " + selectedCandidateTerm); if
	 * (ontologyLookupChildrenQueue.remainingCapacity() == 0) { CandidateTerm
	 * remove = ontologyLookupChildrenQueue.remove(); logger.info("remove " +
	 * remove.getLabel()); }
	 * ontologyLookupChildrenQueue.offer(selectedCandidateTerm); }
	 * List<CandidateTerm> clipboardTerms =
	 * candidateTermsTable.getModel().getAllTerms(); for (CandidateTerm
	 * candidateTerm : clipboardTerms) { if
	 * (candidateTermsTable.getModel().isInClipboard(candidateTerm) &&
	 * !ontologyLookupChildrenQueue.contains(candidateTerm)) {
	 * logger.info("ADD to queue clipboard term: " + candidateTerm); if
	 * (ontologyLookupChildrenQueue.remainingCapacity() == 0) { CandidateTerm
	 * remove = ontologyLookupChildrenQueue.remove(); logger.info("remove " +
	 * remove.getLabel()); } ontologyLookupChildrenQueue.offer(candidateTerm); }
	 * } } else { // render terms logger.info("RENDER: " +
	 * selectedCandidateTerm); List<OBOLookupTerm> existingChildTerms =
	 * selectedCandidateTerm.getExistingChildTerms(); List<OBOLookupRelation>
	 * existingChildRelations =
	 * selectedCandidateTerm.getExistingChildRelations(); for (OBOLookupRelation
	 * lookupRelation : existingChildRelations) { String oboChildTermID =
	 * lookupRelation.getOboChildTermID(); String oboRelationShipType =
	 * lookupRelation.getOboRelationShipType(); for (OBOLookupTerm childTerm :
	 * existingChildTerms) { if (childTerm.getOboID().equals(oboChildTermID)) {
	 * if (childTerm.getLabel() != null &&
	 * !selectedCandidateTerm.getLabel().equals(childTerm.getLabel())) {
	 * CandidateTerm oboCandidateTerm = new CandidateTerm();
	 * oboCandidateTerm.setGeneratedLabel(childTerm.getLabel());
	 * oboCandidateTerm.addLexicalRepresentation(childTerm.getLabel());
	 * oboCandidateTerm.addType(oboRelationShipType);
	 * oboCandidateTerm.setVisible(true);
	 * synonymousOrChildTerms.add(oboCandidateTerm); } } } } }
	 * 
	 * // render the table for synonym terms and children
	 * this.synonymTermsTable.setTerms(synonymousOrChildTerms); for
	 * (CandidateTerm generatedTerm : synonymousOrChildTerms) { if
	 * (synonymTermsTable.getModel().isInClipboard(generatedTerm)) { // add term
	 * merged from eventually existing term and newly // generated in clipboard,
	 * if already in // there.
	 * synonymTermsTable.getModel().addTermToClipboard(generatedTerm); } } }
	 */

	private void updateTermsTableUsingOntologyLookup(TermsTable pTermsTable) {
		Rectangle visibleRect = pTermsTable.getVisibleRect();
		int firstVisibleRow = pTermsTable.rowAtPoint(new Point(0, visibleRect.y));
		int lastVisibleRow = pTermsTable.rowAtPoint(new Point(0, visibleRect.y + visibleRect.height - 1));
		logger.trace(String.format("UPDATE updateTermsTableUsingOntologyLookup; FEED %s %s", firstVisibleRow, //$NON-NLS-1$
				lastVisibleRow));
		if (firstVisibleRow >= 0
				&& lastVisibleRow >= 0
				&& (candidateTermsTable.getCurrentFirstVisibleRow() != firstVisibleRow || candidateTermsTable
						.getCurrentLastVisibleRow() != lastVisibleRow)) {
			ontologyLookupQueue.clear();
			for (int i = firstVisibleRow; i <= lastVisibleRow; i++) {
				CandidateTerm candidateTerm = pTermsTable.getModel().getTermAt(i);
				ontologyLookupQueue.offer(candidateTerm);
			}
		}
		candidateTermsTable.setCurrentFirstVisibleRow(firstVisibleRow);
		candidateTermsTable.setCurrentLastVisibleRow(lastVisibleRow);
	}

	private void updateTermsTableWithExistingTerms(boolean showExistingTerms) {
		candidateTermsTable.setOnlyShowExistingTerms(showExistingTerms);

		// Scroll and select
		JTableHelper.scrollToTopAndSelectFirst(candidateTermsTable);

		// remove existing lookup queries
		ontologyLookupQueue.clear();
		ontologyLookupChildrenQueue.clear();

		// reinitialize visible table rows
		candidateTermsTable.setCurrentFirstVisibleRow(-1);
		candidateTermsTable.setCurrentLastVisibleRow(-1);

		// restart ontology terms lookup
		updateTermsTableUsingOntologyLookup(candidateTermsTable);
	}

	private void updateDefinitionsTableWithFavicons(DefinitionsTable definitionsTable) {
		Rectangle visibleRect = definitionsTable.getVisibleRect();
		int firstVisibleRow = definitionsTable.rowAtPoint(new Point(0, visibleRect.y));
		int lastVisibleRow = definitionsTable.rowAtPoint(new Point(0, visibleRect.y + visibleRect.height - 1));
		if (firstVisibleRow >= 0
				&& lastVisibleRow >= 0
				&& (definitionsTable.getFirstVisibleRow() != firstVisibleRow || definitionsTable.getLastVisibleRow() != lastVisibleRow)) {
			// Extend rows so that always 10 additional rows are fetched after
			// selection.
			int extendedLastVisibleRow = Math.min(lastVisibleRow + 10, definitionsTable.getModel().getRowCount() - 1);
			// int extendedFirstVisibleRow = Math.min(Math.max(0,
			// firstVisibleRow), definitionsTable.getModel().getRowCount()-1);

			faviconRetrievalQueue.clear();
			for (int i = firstVisibleRow; i <= extendedLastVisibleRow; i++) {
				if (!faviconBitSet.get(i)) {
					CandidateDefinition candidateDefinition = definitionsTable.getModel().getDefinitionAt(i);
					faviconRetrievalQueue.offer(candidateDefinition);
					faviconBitSet.set(i);
				}
			}
		}

		definitionsTable.setFirstVisibleRow(firstVisibleRow);
		definitionsTable.setLastVisibleRow(lastVisibleRow);
	}

	private void setUpOntologyLanguagesSelectionBox() {
		ontologyLocales.put(Locale.ENGLISH.getDisplayLanguage(),
				new Locale(Locale.ENGLISH.getLanguage(), Locale.US.getCountry()));
		ontologyLocales.put(Locale.GERMAN.getDisplayLanguage(Locale.GERMAN), new Locale(Locale.GERMAN.getLanguage(),
				Locale.GERMANY.getCountry()));
		// ontologyLocales.put("French", Locale.FRENCH.getLanguage());
		// ontologyLocales.put("Spanish", new Locale("sp", "es"));
		// ontologyLocales.put("Portuguese", "pt");

		HashMap<Locale, String> reverseMap = new HashMap<Locale, String>(ontologyLocales.size());
		Vector<String> languageLabels = new Vector<String>();
		for (Map.Entry<String, Locale> entry : ontologyLocales.entrySet()) {
			languageLabels.add(entry.getKey());
			reverseMap.put(entry.getValue(), entry.getKey());
		}

		Locale locale = Locale.getDefault();
		if (locale == null || !reverseMap.containsKey(locale)) {
			locale = ontologyLocales.get(Locale.ENGLISH.getDisplayLanguage(Locale.ENGLISH));
		}
		languageSelectionBox = new JComboBox(languageLabels);
		adapter.setLocale(locale);
		languageSelectionBox.setSelectedItem(reverseMap.get(locale));

	}

	private void setUpOntologyLookupSelectionBox() {
		Vector<String> labels = new Vector<String>();
		labels.add("EBI Ontology Lookup Service");
		labels.add("NCBO BioPortal Service");
		ontologyLookupSelectionBox = new JComboBox(labels);
		ontologyLookupSelectionBox.setSelectedItem(labels.get(0));

	}

	private void clearAllDependendOnSelectedTerm() {
		definitionTable.removeAll();
		definitionTable.getColumnModel().getColumn(2)
				.setHeaderValue(Messages.getString("OntologyGenerationComponent.DefinitionsTableHeader")); //$NON-NLS-1$
		definitionTable.updateUI();
		searchDefTextField.getTextField().setText(null);
		editDefArea.setText(null);
		saveDefWarningLabel.setText(""); //$NON-NLS-1$
		saveLabelWarningLabel.setText(""); //$NON-NLS-1$
		editNameTextField.setText(null);
		searchPotentialParentsTextField.getTextField().setText(null);
		ontologyTermsTable.getModel().clearStates();
	}

	private void clearFieldsInTermGenerationTable() {
		candidateTermsTable.removeAllTerms();
		candidateTermsTable.getColumnModel().getColumn(1)
				.setHeaderValue(Messages.getString("OntologyGenerationComponent.TermsTableHeader")); //$NON-NLS-1$
		searchTermsTextField.getTextField().setText(null);
	}

	/*
	 * HELPERS
	 */
	private String putInQuotes(String term) {
		StringBuffer qBuffer = new StringBuffer();
		qBuffer.append("\""); //$NON-NLS-1$
		qBuffer.append(term);
		qBuffer.append("\""); //$NON-NLS-1$
		return qBuffer.toString();
	}

	/**
	 * Replace invalid character (prepare text to send with Axis)
	 * 
	 * @param string
	 * @param builder
	 */
	private static final String prepareTextReplaceInvalidCharacter(String string) {
		StringBuilder builder = new StringBuilder();
		for (int i = 0; i < string.length(); i++) {
			char c = string.charAt(i);
			if ((c < 0x001F || (c >= 0x007F && c <= 0x00A0)) && c != 0x000A) {
				builder.append(' ');
			} else {
				builder.append(c);
			}
		}
		return builder.toString();
	}

	public void updateOnOntologyTermSelectionChange(CandidateTerm candidateTerm) {
		logger.trace("UPDATE updateInputFieldsForSelectedLinkedObjectLabel() for :" + candidateTerm.getExistingLabel()); //$NON-NLS-1$
		inputPubMedQueryField.setText(candidateTerm.getExistingLabel());
		inputWebQueryField.setText(candidateTerm.getExistingLabel());
		inputDefinitionGenerationField.setText(candidateTerm.getExistingLabel());
		// FIXME set again in updateAllDependedOnSelectedCandidateTerm() as
		// candidateTerm.getLabel()

		clearFieldsInTermGenerationTable();
		selectedCandidateTerm = candidateTerm;
		candidateTermCache.addTerm(candidateTerm);
		clearAllDependendOnSelectedTerm();

		candidateTermsTable.setTerms(Arrays.asList(candidateTerm));
		candidateTermsTable.getSelectionModel().setSelectionInterval(0, 1);
	}

	private void updateParentAsTermFromDefinition(CandidateTerm pSelectedCandidateTerm, TermsTable termsTable,
			AbstractOntologyTermsTable<T, R> ontologyTermsTable, DefinitionsTable definitionsTable) {
		logger.trace("UPDATE TERMS FROM DEFINITION for :" + pSelectedCandidateTerm); //$NON-NLS-1$

		// clear terms from definition
		ontologyTermsTable.getModel().clearTermsFromDefinitions();

		if (null == pSelectedCandidateTerm) {
			int row = termsTable.rowAtPoint(termsTable.getMousePosition());
			pSelectedCandidateTerm = termsTable.getModel().getTermAt(row);
			logger.warn("Selection lost in terms table, recovered though mouse position"); //$NON-NLS-1$
		}

		// process user defined definition
		if (null != pSelectedCandidateTerm.getUserDefinedDefinition()) {
			String definitionalContext = pSelectedCandidateTerm.getUserDefinedDefinition();
			String[] split = definitionalContext.split(" is a | a | are | are a | are an | are the ", 2); //$NON-NLS-1$
			if (split.length == 2) {
				definitionalContext = split[1].trim();
			}
			List<String> ids = searchForOntologyTermsInStrings(Collections.singletonList(definitionalContext), true);
			int rank = 1;
			for (String someId : ids) {
				ontologyTermsTable.getModel().addFromUserDefinedDefinition(someId, rank);
				rank++;
			}
		}

		// process generated definitions
		List<CandidateDefinition> definitions = definitionsTable.getModel().getDefinitions();
		Set<String> definitionStringsToCheck = new HashSet<String>(definitions.size());
		for (CandidateDefinition definition : definitions) {
			String definitionalContext = definition.getDefinitionalContext();
			if (definitionalContext != null && definitionalContext.length() < definition.getDefinition().length()) {
				// TODO only use definitions where the A is B with C is well
				// matched, meaning the definition starts with
				// the term followed by a form of be
				// if (definition.getDefinition().indexOf(definitionalContext)
				// <= pSelectedCandidateTerm.getLabel()
				// .length() + 30) {
				definitionStringsToCheck.add(definitionalContext);
				// }
			}
		}
		if (definitionStringsToCheck.size() > 0) {
			List<String> ids = searchForOntologyTermsInStrings(definitionStringsToCheck, false);
			int rank = 1;
			for (String someId : ids) {
				ontologyTermsTable.getModel().addFromCandidateDefinition(someId, rank);
				rank++;

			}
		}
	}

	/**
	 * TODO Definitions should be updated, but grouping should take place in the
	 * DefinitionTableModel on update of any definition. This will ensure, that
	 * all definitions are always grouped
	 * 
	 * @param def
	 * @param defList
	 * @param index
	 */
	public static void organizeDefinition(DefinitionContainer def, List<CandidateDefinition> defList, int index) {
		if (def == null) {
			logger.debug("A retrieved definition was null"); //$NON-NLS-1$
			return;
		}

		boolean duplicateDefinition = false;

		String defStr = def.getDefinition();
		if (defStr.endsWith(" ...")) { //$NON-NLS-1$
			defStr = defStr.substring(0, defStr.length() - 4);
		}

		for (int i = 0; i < defList.size(); i++) {
			CandidateDefinition candDef = defList.get(i);

			String candDefStr = candDef.getDefinition();
			if (candDefStr.endsWith(" ...")) { //$NON-NLS-1$
				candDefStr = candDefStr.substring(0, candDefStr.length() - 4);
			}

			if (defStr.equals(candDefStr)) {
				candDef.addURL(def.getUrl());
				candDef.addCachedURL(def.getCachedURL());
				duplicateDefinition = true;
			} else if (candDefStr.contains(defStr) || defStr.contains(candDefStr)) {
				duplicateDefinition = true;

				boolean duplicateAlternativeDefinition = false;
				if (candDef.getAlternativeDefinitions() != null) {
					for (CandidateDefinition definition : candDef.getAlternativeDefinitions()) {
						// Try to find identical alternative
						// definition.
						if (definition.getDefinition().equals(def.getDefinition())) {
							duplicateAlternativeDefinition = true;

							definition.addURL(def.getUrl());
							definition.addCachedURL(def.getCachedURL());
						}
					}
				}

				// If no identical alternative definition is
				// found, add a new alternative definition.
				if (!duplicateAlternativeDefinition) {
					final CandidateDefinition candidateDefinition = new CandidateDefinition(index, def.getDefinition(),
							def.getFormattedDefinition(), def.getUrl(), def.getCachedURL(), def.getParentTermCount(),
							false);

					if (def.getDefinition().length() > candDef.getDefinition().length()) {
						// swap candidateDefinition and
						// alternative Definition
						candidateDefinition.addAlternativeDefinition(candDef);
						if (candDef.getAlternativeDefinitions() != null) {
							for (CandidateDefinition candDefAltDef : candDef.getAlternativeDefinitions()) {
								candidateDefinition.addAlternativeDefinition(candDefAltDef);
							}
							candDef.resetAlternativeDefinitions();
						}
						// candDef.removeListeners();
						int pos = defList.indexOf(candDef);
						defList.remove(candDef);

						defList.add(pos, candidateDefinition);

						// candidateDefinition.addListener(new
						// UpdateListenerInterface()
						// {
						// public void update()
						// {
						// updateParentAsTermFromDefinition(selectedCandidateTerm,
						// candidateTermsTable, ontologyTermsTable,
						// definitionTable);
						// }
						// });

					} else {
						candDef.addAlternativeDefinition(candidateDefinition);
					}
				}
			}
		}
		// Otherwise, add new definition to list.
		if (!duplicateDefinition) {
			final CandidateDefinition candidateDefinition = new CandidateDefinition(index, def.getDefinition(),
					def.getFormattedDefinition(), def.getUrl(), def.getCachedURL(), def.getParentTermCount(), false);
			index++;
			// candidateDefinition.addListener(new UpdateListenerInterface()
			// {
			// public void update()
			// {
			// updateParentAsTermFromDefinition(selectedCandidateTerm,
			// candidateTermsTable, ontologyTermsTable, definitionTable);
			// }
			// });
			defList.add(candidateDefinition);
		}
	}

	/**
	 * Create ngrams contained in the beginning of the texts in the specified
	 * list and looking up the term these ngrams match.
	 * 
	 * @param strings
	 * @return
	 */
	private List<String> searchForOntologyTermsInStrings(Collection<String> strings, boolean generatedAllNGrams) {
		int RANGE_FOR_PARENT_IN_DEFINITION = 5;

		List<String> queries = new ArrayList<String>();
		List<String> queriesHead = new ArrayList<String>();
		Set<String[]> setOfArrays = new HashSet<String[]>(strings.size());

		for (String string : strings) {
			setOfArrays.add(string.trim().split(" |-")); //$NON-NLS-1$
		}

		if (!generatedAllNGrams) {
			for (String[] stringArray : setOfArrays) {
				for (int i = 0; i < RANGE_FOR_PARENT_IN_DEFINITION; i++) {
					StringBuffer buffer = new StringBuffer();
					for (int k = i; k < stringArray.length && k < RANGE_FOR_PARENT_IN_DEFINITION; k++) {
						if (k < stringArray.length) {
							buffer.append(stringArray[k]);
							buffer.append(" "); //$NON-NLS-1$
						}
					}
					String trim = buffer.toString().trim();
					if (trim.length() > 0) {
						queriesHead.add(trim);
					}
				}
			}
		} else {
			for (String[] stringArray : setOfArrays) {
				for (int i = 0; i < stringArray.length && i < RANGE_FOR_PARENT_IN_DEFINITION; i++) {
					for (int j = 0; j < stringArray.length && j < RANGE_FOR_PARENT_IN_DEFINITION; j++) {
						if (i <= j) {
							StringBuffer buffer = new StringBuffer();
							for (int k = i; k <= j; k++) {
								if (k < stringArray.length) {
									buffer.append(stringArray[k]);
									buffer.append(" "); //$NON-NLS-1$
								}
							}
							String trim = buffer.toString().trim();
							if (trim.endsWith(".") || trim.endsWith(":") || trim.endsWith(",") || trim.endsWith("-")
									|| trim.endsWith(";")) {
								// remove trailing punctuation
								trim = trim.substring(0, trim.length() - 1);
							}
							if (trim.length() > 0) {
								queriesHead.add(trim);
							}
						}
					}
				}
			}
		}
		queries.addAll(queriesHead);
		return adapter.lookupOntologyTermIdsFromIndex(queries);
	}

	/**
	 * Updates the <code>definition</code>, based on the selected and user
	 * defined definitions.
	 */
	public void updateEditDefArea(String newDefinition) {
		StringBuffer definition = new StringBuffer();
		deleteDefButton.setEnabled(false);
		// deleteDefButton.getParent().repaint();

		if (newDefinition != null) {
			definition.append(newDefinition);
			updateEditDefAreaString(definition.toString());
		} else {
			if (selectedCandidateTerm != null) {
				if (selectedCandidateTerm.getUserDefinedDefinition() != null) {
					definition.append(selectedCandidateTerm.getUserDefinedDefinition());
				}
			}
			String definitionForExistingTerm = null;
			if (selectedCandidateTerm.isInLoadedOntology()) {
				definitionForExistingTerm = selectedCandidateTerm.getExistingOntologyClass().getDefinition(
						adapter.getLanguage());
			}
			if (null != definitionForExistingTerm) {
				deleteDefButton.setEnabled(true);
				if (!definition.toString().equals(definitionForExistingTerm)) {
					if (definition.length() > 0) {
						definition.append("\n-----\n"); //$NON-NLS-1$
					}
					definition.append(definitionForExistingTerm);
				}
			}
			editDefArea.setText(definition.toString());
		}
	}

	public void updateEditDefAreaString(String newDefinition) {
		StringBuffer defString = new StringBuffer();
		String defAreaText = editDefArea.getText();
		defString.append(defAreaText.trim());

		String separator = "----" + Messages.getString("OntologyGenerationComponent.NewlyGenerationDefinitionSeparator.text") + "-----"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		if (!defAreaText.contains(separator)) {
			defString.append("\n"); //$NON-NLS-1$
			defString.append(separator);
			defString.append("\n- "); //$NON-NLS-1$
			defString.append(newDefinition.trim());

		} else {
			defString.append("\n- "); //$NON-NLS-1$
			defString.append(newDefinition.trim());
		}

		editDefArea.setText(defString.toString());

	}

	public void setTextSelectedOntologyTermField(String s) {
		selectedLinkedObjectField.setText(s);
	}

	public OntologyLookupManagerPortTypeProxy getOntologyLookupProxy() {
		return ontoLookupProxy;
	}

	public String getId() {
		return id;
	}

	public CandidateTerm getSelectedCandidateTerm() {
		return selectedCandidateTerm;
	}

	public DefinitionsTable getDefinitionsTable() {
		return definitionTable;
	}

	public AbstractOntologyTermsTable<T, R> getOntologyTermsTable() {
		return ontologyTermsTable;
	}

	public TermsTable getTermsTable() {
		return candidateTermsTable;
	}

	/**
	 * Lays out the GUI
	 */
	public JComponent buildGUI() {
		// Set the layout of main window
		guiComponent.setLayout(new BoxLayout(guiComponent, BoxLayout.Y_AXIS));
		//
		JPanel allStepsPanel = new JPanel();
		allStepsPanel.setLayout(new BoxLayout(allStepsPanel, BoxLayout.Y_AXIS));

		//
		// 1-Term Generation panel
		//
		termGenerationPanel = new JPanel(new BorderLayout(7, 7));

		// Set the Border with Bold font face
		final TitledBorder titledBorderTermGenerationPanel = new TitledBorder(
				Messages.getString("OntologyGenerationComponent.TermGenerationPanel.titledBorder")); //$NON-NLS-1$
		titledBorderTermGenerationPanel.setTitleFont(new Font(titledBorderTermGenerationPanel.getTitleFont()
				.getFontName(), Font.BOLD, 18));
		titledBorderTermGenerationPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		// 1 sub panels
		// Input Panel

		JPanel inputPanel = new JPanel();
		inputPanel.setLayout(new BoxLayout(inputPanel, BoxLayout.X_AXIS));
		inputPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		inputPanel.setAlignmentY(Component.BOTTOM_ALIGNMENT);

		JTabbedPane inputTabPanel = new JTabbedPane();
		clipboardTabPanel = new JTabbedPane();
		clipboardTabPanel.setMaximumSize(new Dimension(200, 200));

		GridBagConstraints inputTextAreaConstraints = new GridBagConstraints();
		inputTextAreaConstraints.fill = GridBagConstraints.BOTH;
		inputTextAreaConstraints.ipady = 0; // make this component tall
		inputTextAreaConstraints.ipadx = 100;
		inputTextAreaConstraints.weightx = 0.5;
		inputTextAreaConstraints.gridwidth = 3;
		inputTextAreaConstraints.gridx = 0;
		inputTextAreaConstraints.gridy = 0;
		inputTextArea = new JTextArea(3, 4);
		JScrollPane inputTextScrollPane = new JScrollPane(inputTextArea);
		inputTextArea.setLineWrap(true);
		inputTextArea.setWrapStyleWord(true);
		inputTextScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

		// Primary input panel
		JPanel primaryInputBoardPanelForPubMed = new JPanel();
		primaryInputBoardPanelForPubMed.setLayout(new BoxLayout(primaryInputBoardPanelForPubMed, BoxLayout.X_AXIS));

		// add components
		JLabel inputPubMedLabel = new JLabel(Messages.getString("OntologyGenerationComponent.QueryPubMed.label")); //$NON-NLS-1$
		Dimension spacer = new Dimension(3, 3);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(inputPubMedLabel);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(inputPubMedQueryField);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPubMed.add(generateTermsFromPubMedButton);
		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));

		JPanel primaryInputBoardPanelForWeb = new JPanel();
		primaryInputBoardPanelForWeb.setLayout(new BoxLayout(primaryInputBoardPanelForWeb, BoxLayout.X_AXIS));

		JLabel inputWebLabel = new JLabel(Messages.getString("OntologyGenerationComponent.QueryWeb.label")); //$NON-NLS-1$
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForWeb.add(inputWebLabel);
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForWeb.add(inputWebQueryField);
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForWeb.add(generateTermsFromWebButton);
		primaryInputBoardPanelForWeb.add(Box.createRigidArea(spacer));

		JPanel primaryInputBoardPanelForSiblings = new JPanel();
		primaryInputBoardPanelForSiblings.setLayout(new BoxLayout(primaryInputBoardPanelForSiblings, BoxLayout.X_AXIS));

		JLabel inputSiblingLabel = new JLabel(Messages.getString("OntologyGenerationComponent.SeedTerms.label")); //$NON-NLS-1$
		primaryInputBoardPanelForSiblings.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForSiblings.add(inputSiblingLabel);
		primaryInputBoardPanelForSiblings.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForSiblings.add(inputSeedTermField1);
		primaryInputBoardPanelForSiblings.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForSiblings.add(inputSeedTermField2);
		primaryInputBoardPanelForSiblings.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForSiblings.add(inputSeedTermField3);
		primaryInputBoardPanelForSiblings.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForSiblings.add(generateSiblingsButton);
		primaryInputBoardPanelForSiblings.add(Box.createRigidArea(spacer));
		generateSiblingsButton.setEnabled(false);

		JPanel primaryInputBoardPanelForText = new JPanel();
		primaryInputBoardPanelForText.setLayout(new BoxLayout(primaryInputBoardPanelForText, BoxLayout.X_AXIS));
		// Set the Border with Bold font face
		// TitledBorder titledBorderPrimaryInputBoardPanelForText = new
		// TitledBorder("Generated From Text");
		// titledBorderPrimaryInputBoardPanelForText.setTitleFont(new
		// Font(titledBorderTermGenerationPanel.getTitleFont().getFontName(),
		// Font.PLAIN, 10));
		// titledBorderPrimaryInputBoardPanelForText.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		JLabel inputTextLabel = new JLabel(Messages.getString("OntologyGenerationComonent.PasteText.label")); //$NON-NLS-1$
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(inputTextLabel);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(inputTextScrollPane, inputTextAreaConstraints);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForText.add(generateTermsFromTextButton);
		primaryInputBoardPanelForText.add(Box.createRigidArea(spacer));

		// primary input panel for Folder of pdf files
		JPanel primaryInputBoardPanelForPdfFolder = new JPanel();
		primaryInputBoardPanelForPdfFolder
				.setLayout(new BoxLayout(primaryInputBoardPanelForPdfFolder, BoxLayout.X_AXIS));

		JLabel inputFolderLabel = new JLabel(Messages.getString("OntologyGenerationComponent.InputFolder.label")); //$NON-NLS-1$
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(inputFolderLabel);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(inputFolderLocationField);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(folderChooseButton);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));
		primaryInputBoardPanelForPdfFolder.add(generateTermsFromFolderButton);
		primaryInputBoardPanelForPdfFolder.add(Box.createRigidArea(spacer));

		inputTabPanel
				.addTab("PubMed", null, primaryInputBoardPanelForPubMed, Messages.getString("OntologyGenerationComponent.PubMedTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		inputTabPanel
				.addTab("Web", null, primaryInputBoardPanelForWeb, Messages.getString("OntologyGenerationComponent.WebTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		inputTabPanel
				.addTab("Siblings", null, primaryInputBoardPanelForSiblings, Messages.getString("OntologyGenerationComponent.SiblingTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		inputTabPanel
				.addTab("Text", null, primaryInputBoardPanelForText, Messages.getString("OntologyGenerationComponent.TextTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		inputTabPanel
				.addTab("PDF", null, primaryInputBoardPanelForPdfFolder, Messages.getString("OntologyGenerationComponent.PDFTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		inputPanel.add(inputTabPanel);
		inputPanel.setBorder(BorderFactory.createEmptyBorder(7, 0, 0, 0));

		// Clipboard panel
		JPanel clipboardPanel = new JPanel();

		clipboardPanel.setLayout(new GridLayout(2, 2, 6, 6));
		clipboardPanel.add(showClipboardButton);
		clipboardPanel.add(loadClipboardButton);
		clipboardPanel.add(clearClipboardButton);
		clipboardPanel.add(saveClipboardButton);

		primaryInputBoardPanelForPubMed.add(Box.createRigidArea(spacer));
		clipboardTabPanel.addTab(
				Messages.getString("OntologyGenerationComponent.ClipboardTabPanel.label"), clipboardPanel); //$NON-NLS-1$

		inputPanel.add(Box.createRigidArea(spacer));
		inputPanel.add(clipboardTabPanel);

		if (inputPubMedQueryField.getText().trim().length() == 0) {
			generateTermsFromPubMedButton.setEnabled(false);
		} else {
			generateTermsFromPubMedButton.setEnabled(true);
		}

		if (inputWebQueryField.getText().trim().length() == 0) {
			generateTermsFromWebButton.setEnabled(false);
		} else {
			generateTermsFromWebButton.setEnabled(true);
		}

		if (inputTextArea.getText().trim().length() == 0) {
			generateTermsFromTextButton.setEnabled(false);
		} else {
			generateTermsFromTextButton.setEnabled(true);
		}

		if (inputFolderLocationField.getText().trim().length() == 0) {
			generateTermsFromFolderButton.setEnabled(false);
		} else {
			generateTermsFromFolderButton.setEnabled(true);
		}

		// Filter Term Panel to be added to InputPanel containing regular
		// expressions filtration
		JPanel searchTermPanel = new JPanel();
		searchTermPanel.setLayout(new BoxLayout(searchTermPanel, BoxLayout.Y_AXIS));
		searchTermPanel.setAlignmentX(Component.TOP_ALIGNMENT);

		JPanel searchTermUpperPanel = new JPanel();
		searchTermUpperPanel.setLayout(new BoxLayout(searchTermUpperPanel, BoxLayout.X_AXIS));
		searchTermUpperPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		searchTermsTextField = new FilterTextField();
		searchTermsTextField.setSize(200, 25);
		searchTermsTextField.setMaximumSize(new Dimension(200, 25));
		searchTermsTextField.setPreferredSize(new Dimension(200, 25));
		searchTermUpperPanel.add(searchTermsTextField);

		onlyShowExistingTerms.setText(Messages
				.getString("OntologyGenerationComponent.onlyShowExistingTermsCheckbox.label")); //$NON-NLS-1$
		onlyShowExistingTerms.setBackground(guiComponent.getBackground());
		onlyShowExistingTerms.setToolTipText(Messages
				.getString("OntologyGenerationComponent.onlyShowExistingTermsCheckbox.tooltip")); //$NON-NLS-1$
		searchTermUpperPanel.add(Box.createRigidArea(spacer));
		searchTermUpperPanel.add(onlyShowExistingTerms);

		searchTermPanel.add(Box.createRigidArea(new Dimension(7, 0)));
		searchTermPanel.add(searchTermUpperPanel);
		searchTermPanel.add(Box.createRigidArea(spacer));

		// Add the subPanels to the Term Generation Panel
		termGenerationPanel.add(inputPanel, BorderLayout.NORTH);

		candidateTermsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		candidateTermsTable.setPreferredScrollableViewportSize(new Dimension(200, 200));

		JPanel termsTableContainer = new JPanel();
		termsTableContainer.setLayout(new BoxLayout(termsTableContainer, BoxLayout.X_AXIS));
		termsTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		scrollPaneForTermsTable = new JScrollPane(candidateTermsTable);
		termsTableContainer.add(scrollPaneForTermsTable);
		termsTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		termGenerationPanel.setBorder(titledBorderTermGenerationPanel);
		termGenerationPanel.add(termsTableContainer, BorderLayout.CENTER);
		termGenerationPanel.add(searchTermPanel, BorderLayout.SOUTH);

		//
		// 2 Definition Generation panel
		//
		definitionGenerationPanel = new JPanel(new BorderLayout(7, 7));
		// set the Border with Bold font face
		final TitledBorder titledBorderDefPanel = new TitledBorder(
				Messages.getString("OntologyGenerationComponent.DefinitionGenerationPanel.titledBorder")); //$NON-NLS-1$
		titledBorderDefPanel.setTitleFont(new Font(titledBorderDefPanel.getTitleFont().getFontName(), Font.BOLD, 18));
		titledBorderDefPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		definitionGenerationPanel.setBorder(titledBorderDefPanel);

		JPanel manualDefGenerationPanel = new JPanel();
		manualDefGenerationPanel.setLayout(new BoxLayout(manualDefGenerationPanel, BoxLayout.X_AXIS));

		JLabel manualDefGenLabel = new JLabel(
				Messages.getString("OntologyGenerationComponent.FindDefinitionsFor.label")); //$NON-NLS-1$
		inputDefinitionGenerationField = new JTextField();
		inputDefinitionGenerationField.setSize(200, 25);
		inputDefinitionGenerationField.setMaximumSize(new Dimension(200, 25));
		inputDefinitionGenerationField.setPreferredSize(new Dimension(200, 25));
		inputDefinitionGenerationField.setToolTipText(Messages
				.getString("OntologyGenerationComponent.ManualDefinitionField.tooltip")); //$NON-NLS-1$
		generateManualDefinitionButton.setEnabled(false);
		manualDefGenerationPanel.add(Box.createRigidArea(new Dimension(7, 0)));
		manualDefGenerationPanel.add(manualDefGenLabel);
		manualDefGenerationPanel.add(inputDefinitionGenerationField);
		manualDefGenerationPanel.add(Box.createRigidArea(spacer));
		manualDefGenerationPanel.add(generateManualDefinitionButton);

		//
		// 2-Definition Generation sub panels
		//
		// Filter definitions
		JPanel searchDefPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		searchDefTextField = new FilterTextField();
		searchDefTextField.setSize(200, 25);
		searchDefTextField.setMaximumSize(new Dimension(200, 25));
		searchDefTextField.setPreferredSize(new Dimension(200, 25));
		searchDefPanel.add(searchDefTextField);

		JPanel editDefPanel = new JPanel(new GridBagLayout());
		editDefPanel.setBorder(new TitledBorder(Messages
				.getString("OntologyGenerationComponent.EditDefinition.titledBorder"))); //$NON-NLS-1$

		GridBagConstraints editDefAreaConstraints = new GridBagConstraints();
		editDefAreaConstraints.fill = GridBagConstraints.BOTH;
		editDefAreaConstraints.ipady = 0; // make this component tall
		editDefAreaConstraints.ipadx = 100;
		editDefAreaConstraints.weightx = 0.5;
		editDefAreaConstraints.gridwidth = 3;
		editDefAreaConstraints.gridx = 0;
		editDefAreaConstraints.gridy = 0;

		editDefArea = new JTextArea();
		JScrollPane editDefAreaScrollPane = new JScrollPane(editDefArea);
		editDefArea.setLineWrap(true);
		editDefArea.setWrapStyleWord(true);
		editDefAreaScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		editDefAreaScrollPane.setPreferredSize(new Dimension(200, 80));
		editDefPanel.add(editDefAreaScrollPane, editDefAreaConstraints);

		GridBagConstraints saveDefButtonConstraints = new GridBagConstraints();
		saveDefButtonConstraints.fill = GridBagConstraints.HORIZONTAL;
		saveDefButtonConstraints.ipady = 0; // reset to default
		saveDefButtonConstraints.weighty = 0.0; // request any extra vertical
		saveDefButtonConstraints.anchor = GridBagConstraints.PAGE_END; // bottom
		saveDefButtonConstraints.insets = new Insets(5, 100, 0, 0); // top
		saveDefButtonConstraints.gridwidth = 1; // 1 columns wide
		saveDefButtonConstraints.gridheight = 2;
		saveDefButtonConstraints.gridx = 0; // first column
		saveDefButtonConstraints.gridy = 1; // second row
		editDefPanel.add(saveDefButton, saveDefButtonConstraints);

		GridBagConstraints deleteDefButtonConstraints = new GridBagConstraints();
		deleteDefButtonConstraints.fill = GridBagConstraints.HORIZONTAL;
		deleteDefButtonConstraints.ipady = 0; // reset to default
		deleteDefButtonConstraints.weighty = 0.0; // request any extra vertical
		deleteDefButtonConstraints.anchor = GridBagConstraints.CENTER; // bottom
		deleteDefButtonConstraints.insets = new Insets(5, 10, 0, 0); // top
		deleteDefButtonConstraints.gridwidth = 1; // 1 columns wide
		deleteDefButtonConstraints.gridheight = 2;
		deleteDefButtonConstraints.gridx = 1; // second column
		deleteDefButtonConstraints.gridy = 1; // second row
		deleteDefButton.setEnabled(false);
		editDefPanel.add(deleteDefButton, deleteDefButtonConstraints);

		GridBagConstraints saveDefWarningLabelConstraints = new GridBagConstraints();
		saveDefWarningLabelConstraints.insets = new Insets(5, 10, 0, 0); // top
		saveDefWarningLabelConstraints.gridheight = 2;
		saveDefWarningLabelConstraints.anchor = GridBagConstraints.CENTER;
		saveDefWarningLabelConstraints.gridx = 2; // third column
		saveDefWarningLabelConstraints.gridy = 1; // second row

		saveDefWarningLabel.setFont(saveDefWarningLabel.getFont().deriveFont(Font.BOLD));
		saveDefWarningLabel.setForeground(Color.red);
		saveDefWarningLabel.setPreferredSize(new Dimension(140, 15));

		editDefPanel.add(saveDefWarningLabel, saveDefWarningLabelConstraints);

		// southPanel will contain filterDefPanel and editorPanel
		JPanel southDefPanel = new JPanel();
		southDefPanel.setLayout(new BoxLayout(southDefPanel, BoxLayout.Y_AXIS));
		southDefPanel.add(searchDefPanel);
		southDefPanel.add(editDefPanel);

		// Add the subPanels to the Definition Generation Panel
		definitionTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		definitionTable.setMinimumPreferredScrollableViewportHeight(40);
		definitionTable.setMaximumPreferredScrollableViewportHeight(200);
		definitionTable.setPreferredScrollableViewportSize(new Dimension(200, 40));

		JPanel definitionTableContainer = new JPanel();
		definitionTableContainer.setLayout(new BoxLayout(definitionTableContainer, BoxLayout.X_AXIS));
		definitionTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		scrollPaneForDefinitionsTable = new JScrollPane(definitionTable);
		definitionTableContainer.add(scrollPaneForDefinitionsTable);
		definitionTableContainer.add(Box.createRigidArea(new Dimension(7, 0)));
		definitionGenerationPanel.add(Box.createRigidArea(new Dimension(0, 5)), BorderLayout.NORTH);
		manualDefGenerationPanel.setBorder(BorderFactory.createEmptyBorder(10, 0, 0, 0));
		definitionGenerationPanel.add(manualDefGenerationPanel, BorderLayout.NORTH);
		definitionGenerationPanel.add(definitionTableContainer, BorderLayout.CENTER);
		definitionGenerationPanel.add(southDefPanel, BorderLayout.SOUTH);

		//
		// 3-Add child to ontology Panel and 3 sub panels
		//
		final TitledBorder titledBorderAddToOntologyPanel = new TitledBorder(
				Messages.getString("OntologyGenerationComponent.OntologyTermsTablePanel.titledBorder")); //$NON-NLS-1$
		titledBorderAddToOntologyPanel.setTitleFont(new Font(titledBorderAddToOntologyPanel.getTitleFont()
				.getFontName(), Font.BOLD, 18));
		titledBorderAddToOntologyPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));

		addToOntologyPanel = new JPanel();
		addToOntologyPanel.setLayout(new BoxLayout(addToOntologyPanel, BoxLayout.Y_AXIS));

		JPanel candidateToAddPanel = new JPanel(new GridBagLayout());
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.insets = new Insets(0, 7, 0, 0);

		{
			JLabel label = new JLabel(Messages.getString("OntologyGenerationComponent.AddTermLabel.label")); //$NON-NLS-1$
			constraints.gridx = 0;
			constraints.gridy = 0;
			constraints.weightx = 0;
			candidateToAddPanel.add(label, constraints);
		}

		editNameTextField.setToolTipText(Messages.getString("OntologyGenerationComponent.ManualTermNameField.tooltip")); //$NON-NLS-1$
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.weightx = 0;
		candidateToAddPanel.add(editNameTextField, constraints);

		constraints.gridx = 2;
		constraints.gridy = 0;
		constraints.weightx = 0;
		candidateToAddPanel.add(saveLabelButton, constraints);

		saveLabelWarningLabel.setFont(saveDefWarningLabel.getFont().deriveFont(Font.BOLD));
		saveLabelWarningLabel.setForeground(Color.red);
		saveLabelWarningLabel.setPreferredSize(new Dimension(140, 15));
		constraints.gridx = 3;
		constraints.gridy = 0;
		constraints.weightx = 0;
		candidateToAddPanel.add(saveLabelWarningLabel, constraints);

		checkboxIncludeChildren = new JCheckBox(
				Messages.getString("OntologyGenerationComponent.IncludeChildrenCheckbox")); //$NON-NLS-1$
		checkboxIncludeChildren.setBackground(guiComponent.getBackground());
		checkboxIncludeChildren.setToolTipText(Messages
				.getString("OntologyGenerationComponent.IncludeChildrenCheckbox.tooltip")); //$NON-NLS-1$
		constraints.gridx = 4;
		constraints.gridy = 0;
		constraints.weightx = 0.5;
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.insets = new Insets(0, 0, 0, 7);
		candidateToAddPanel.add(checkboxIncludeChildren, constraints);

		/*
		 * checkboxIncludeBranch = new JCheckBox();
		 * candidateToAddPanel.add(checkboxIncludeBranch); JLabel label3 = new
		 * JLabel("include sub-branch"); candidateToAddPanel.add(label3);
		 */
		JPanel oboClassTableHeaderPanel = new JPanel(new BorderLayout());

		JPanel parentLabelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JLabel potentialParentTermLabel = new JLabel(
				Messages.getString("OntologyGenerationComponent.PotentialParentTermsLabel.label")); //$NON-NLS-1$
		parentLabelPanel.add(potentialParentTermLabel);

		oboClassTableHeaderPanel.add(parentLabelPanel, BorderLayout.WEST);

		JPanel oboClassTablePanelContainer = new JPanel();
		oboClassTablePanelContainer.setLayout(new BoxLayout(oboClassTablePanelContainer, BoxLayout.Y_AXIS));
		JPanel oboClassTablePanel = new JPanel();
		oboClassTablePanel.setLayout(new BoxLayout(oboClassTablePanel, BoxLayout.X_AXIS));

		JScrollPane scrollPaneForPotentialParents = new JScrollPane(this.ontologyTermsTable);
		oboClassTablePanel.add(Box.createRigidArea(new Dimension(7, 0)));
		oboClassTablePanel.add(scrollPaneForPotentialParents);
		addToOntologyButton = new JButton(
				Messages.getString("OntologyGenerationComponent.AddTermToOntologyButton.label")); //$NON-NLS-1$
		addToOntologyButton.setMaximumSize(new Dimension(110, 330));
		addToOntologyButton.setToolTipText(Messages
				.getString("OntologyGenerationComponent.AddTermToOntologyButton.tooltip")); //$NON-NLS-1$
		oboClassTablePanel.add(Box.createRigidArea(new Dimension(7, 0)));
		oboClassTablePanel.add(this.addToOntologyButton);
		oboClassTablePanel.add(Box.createRigidArea(new Dimension(7, 0)));

		// Filter Term Panel to be added to InputPanel containing regular
		// expressions filtration

		JPanel searchPotentialParentTermsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		searchPotentialParentsTextField = new FilterTextField();
		searchPotentialParentsTextField.setSize(200, 25);
		searchPotentialParentsTextField.setMaximumSize(new Dimension(200, 25));
		searchPotentialParentsTextField.setPreferredSize(new Dimension(200, 25));
		searchPotentialParentTermsPanel.add(searchPotentialParentsTextField);

		checkboxShowTickedParents = new JCheckBox(
				Messages.getString("OntologyGenerationComponent.ShowTickedParentTermsOnlyCheckbox.label")); //$NON-NLS-1$
		checkboxShowTickedParents.setBackground(guiComponent.getBackground());
		checkboxShowTickedParents.setToolTipText(Messages
				.getString("OntologyGenerationComponent.ShowTickedParentTermsOnlyCheckbox.tooltip")); //$NON-NLS-1$
		checkboxShowTickedParents.setEnabled(true);
		searchPotentialParentTermsPanel.add(checkboxShowTickedParents);

		oboClassTablePanelContainer.add(oboClassTableHeaderPanel);
		oboClassTablePanelContainer.add(oboClassTablePanel);

		addToOntologyPanel.setBorder(titledBorderAddToOntologyPanel);

		addToOntologyPanel.add(candidateToAddPanel, BorderLayout.NORTH);
		addToOntologyPanel.add(oboClassTablePanelContainer, BorderLayout.CENTER);
		addToOntologyPanel.add(searchPotentialParentTermsPanel, BorderLayout.SOUTH);

		final JPanel termGenerationPanelContainer = new JPanel();
		termGenerationPanelContainer.setLayout(new BoxLayout(termGenerationPanelContainer, BoxLayout.Y_AXIS));
		final JPanel definitonGenerationPanelContainer = new JPanel();
		definitonGenerationPanelContainer.setLayout(new BoxLayout(definitonGenerationPanelContainer, BoxLayout.Y_AXIS));
		final JPanel addToOntologyPanelContainer = new JPanel();
		addToOntologyPanelContainer.setLayout(new BoxLayout(addToOntologyPanelContainer, BoxLayout.Y_AXIS));
		final JPanel termGenerationPanelForTab = new JPanel();
		termGenerationPanelForTab.setLayout(new BoxLayout(termGenerationPanelForTab, BoxLayout.Y_AXIS));
		final JPanel definitionGenerationPanelForTab = new JPanel();
		definitionGenerationPanelForTab.setLayout(new BoxLayout(definitionGenerationPanelForTab, BoxLayout.Y_AXIS));
		final JPanel addToOntologyPanelForTab = new JPanel();
		addToOntologyPanelForTab.setLayout(new BoxLayout(addToOntologyPanelForTab, BoxLayout.Y_AXIS));
		final JPanel preferencesPanelForTab = new JPanel(new BorderLayout());

		final JPanel innerPreferencesPanel = new JPanel(new GridBagLayout());

		// Lookup PANEL
		{
			final JPanel preferenceOntologyLookupPanel = new JPanel();
			JLabel label = new JLabel(Messages.getString("PreferenceTab.OntologyLookupLabel.label")); //$NON-NLS-1$
			preferenceOntologyLookupPanel.add(label);
			label.setLabelFor(ontologyLookupSelectionBox);
			preferenceOntologyLookupPanel.add(ontologyLookupSelectionBox);

			final TitledBorder titleBorder = new TitledBorder(
					Messages.getString("PreferenceTab.OntologyLookupTitleBoarder.label"));
			titleBorder.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
			titleBorder.setTitleFont(titleBorder.getTitleFont().deriveFont(Font.BOLD));
			preferenceOntologyLookupPanel.setBorder(titleBorder);

			GridBagConstraints innerPreferencesPanelConstraints = new GridBagConstraints();
			innerPreferencesPanelConstraints.weightx = 0;
			innerPreferencesPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
			innerPreferencesPanelConstraints.gridx = 0;
			innerPreferencesPanelConstraints.gridy = 0;

			innerPreferencesPanel.add(preferenceOntologyLookupPanel, innerPreferencesPanelConstraints);
		}

		// LANGUAGE PANEL
		{
			final JPanel preferenceLanguangePanel = new JPanel();
			JLabel langLabel = new JLabel(Messages.getString("PreferenceTab.LanguageLabel.label")); //$NON-NLS-1$
			preferenceLanguangePanel.add(langLabel);
			langLabel.setLabelFor(languageSelectionBox);
			preferenceLanguangePanel.add(languageSelectionBox);

			final TitledBorder languageTitleBorder = new TitledBorder(
					Messages.getString("PreferenceTab.LanguageTitleBoarder.label")); //$NON-NLS-1$
			languageTitleBorder.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
			languageTitleBorder.setTitleFont(languageTitleBorder.getTitleFont().deriveFont(Font.BOLD));
			preferenceLanguangePanel.setBorder(languageTitleBorder);

			GridBagConstraints innerPreferencesPanelConstraints = new GridBagConstraints();
			innerPreferencesPanelConstraints.weightx = 0;
			innerPreferencesPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
			innerPreferencesPanelConstraints.gridx = 0;
			innerPreferencesPanelConstraints.gridy = 1;

			innerPreferencesPanel.add(preferenceLanguangePanel, innerPreferencesPanelConstraints);
		}

		// PROXY PANEL URL
		final JPanel preferencesProxyPanel = new JPanel();
		preferencesProxyPanel.setLayout(new BorderLayout());

		final TitledBorder proxyTitleBorder = new TitledBorder(
				Messages.getString("PreferenceTab.ProxyTitleBorder.label")); //$NON-NLS-1$
		proxyTitleBorder.setTitleFont(proxyTitleBorder.getTitleFont().deriveFont(Font.BOLD));
		proxyTitleBorder.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		preferencesProxyPanel.setBorder(proxyTitleBorder);

		JPanel labelPanel = new JPanel(new GridLayout(4, 1));
		JPanel fieldPanel = new JPanel(new GridLayout(4, 1));

		{
			JLabel label = new JLabel(Messages.getString("PreferenceTab.URL.label"), JLabel.RIGHT); //$NON-NLS-1$
			proxyHostTextField = new JTextField(20);
			proxyHostTextField.setText(ProxyInfo.getHost() != null ? ProxyInfo.getHost() : ""); //$NON-NLS-1$
			label.setLabelFor(proxyHostTextField);

			JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			panel.add(proxyHostTextField);

			labelPanel.add(label);
			fieldPanel.add(panel);
		}
		// PROXY PANEL PORT
		{
			JLabel label = new JLabel(Messages.getString("PreferenceTab.Port.label"), JLabel.RIGHT); //$NON-NLS-1$
			proxyPortTextField = new JTextField(5);
			proxyPortTextField.setText(ProxyInfo.getPort() != null ? ProxyInfo.getPort() : ""); //$NON-NLS-1$
			label.setLabelFor(proxyPortTextField);

			JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			panel.add(proxyPortTextField);

			labelPanel.add(label);
			fieldPanel.add(panel);
		}
		// PROXY PANEL USER
		{
			JLabel userLabel = new JLabel(Messages.getString("PreferenceTab.UserLabel.label"), JLabel.RIGHT); //$NON-NLS-1$
			proxyUserTextField = new JTextField(10);
			proxyUserTextField.setText(ProxyInfo.getUsername() != null ? ProxyInfo.getUsername() : ""); //$NON-NLS-1$
			userLabel.setLabelFor(proxyUserTextField);

			JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			panel.add(proxyUserTextField);

			labelPanel.add(userLabel);
			fieldPanel.add(panel);
		}
		// PROXY PANEL PASSWORD
		{
			JLabel label = new JLabel(Messages.getString("PreferenceTab.PasswordLabel.label"), JLabel.RIGHT); //$NON-NLS-1$
			proxyPasswordField = new JPasswordField(10);
			proxyPasswordField.setText(ProxyInfo.getPassword() != null ? ProxyInfo.getPassword() : ""); //$NON-NLS-1$
			label.setLabelFor(proxyPasswordField);

			JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			panel.add(proxyPasswordField);

			labelPanel.add(label);
			fieldPanel.add(panel);
		}

		preferencesProxyPanel.add(labelPanel, BorderLayout.WEST);
		preferencesProxyPanel.add(fieldPanel, BorderLayout.CENTER);

		// RESET & SAVE BUTTON
		{
			proxyResetButton = new JButton(Messages.getString("PreferenceTab.ResetButton.label")); //$NON-NLS-1$
			proxySaveButton = new JButton(Messages.getString("PreferenceTab.SaveButton.label")); //$NON-NLS-1$

			JPanel panel = new JPanel();
			panel.setLayout(new BoxLayout(panel, BoxLayout.LINE_AXIS));
			panel.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 0));
			panel.add(Box.createHorizontalGlue());
			panel.add(proxyResetButton);
			panel.add(proxySaveButton);

			proxySaveButton.setEnabled(false);
			proxyResetButton.setEnabled((ProxyInfo.getHost() != null || ProxyInfo.getPort() != null
					|| ProxyInfo.getUsername() != null || ProxyInfo.getPassword() != null));

			preferencesProxyPanel.add(panel, BorderLayout.SOUTH);

			GridBagConstraints innerPreferencesPanelConstraints = new GridBagConstraints();
			innerPreferencesPanelConstraints.weightx = 0;
			innerPreferencesPanelConstraints.fill = GridBagConstraints.HORIZONTAL;
			innerPreferencesPanelConstraints.gridx = 0;
			innerPreferencesPanelConstraints.gridy = 2;
			innerPreferencesPanel.add(preferencesProxyPanel, innerPreferencesPanelConstraints);
		}
		preferencesPanelForTab.add(innerPreferencesPanel, BorderLayout.NORTH);

		// ABOUT PANEL
		final JPanel aboutPanelForTab = new JPanel();
		aboutPanelForTab.setLayout(new FlowLayout());

		String url = "http://www.biotec.tu-dresden.de/~waechter/obo-edit-ontogen/"; //$NON-NLS-1$
		JScrollPane scrollPaneForHelp = null;
		try {
			JEditorPane htmlPane = new JEditorPane(url);
			htmlPane.setEditable(false);
			scrollPaneForHelp = new JScrollPane(htmlPane);
			scrollPaneForHelp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		} catch (IOException ioe) {
			logger.error("Error displaying " + url); //$NON-NLS-1$
		}
		JPanel splashPanel = BiotecSplashScreen.getSplashPanel();
		aboutPanelForTab.add(splashPanel);

		// TODO scrolling to slow, increase step size
		allStepsPanel.add(termGenerationPanelContainer);
		allStepsPanel.add(Box.createRigidArea(new Dimension(0, 15)));
		allStepsPanel.add(definitonGenerationPanelContainer);
		allStepsPanel.add(Box.createRigidArea(new Dimension(0, 15)));
		allStepsPanel.add(addToOntologyPanelContainer);

		// create scroll panes
		JScrollPane allStepsScrollPane = new JScrollPane(allStepsPanel);
		allStepsScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		allStepsScrollPane.getVerticalScrollBar().setUnitIncrement(10);

		JScrollPane termGenerationScrollPane = new JScrollPane(termGenerationPanelForTab);
		termGenerationScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		termGenerationScrollPane.getVerticalScrollBar().setUnitIncrement(10);

		JScrollPane definitionGenerationScrollPane = new JScrollPane(definitionGenerationPanelForTab);
		definitionGenerationScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		definitionGenerationScrollPane.getVerticalScrollBar().setUnitIncrement(10);

		JScrollPane preferencesScrollPane = new JScrollPane(preferencesPanelForTab);
		preferencesScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		preferencesScrollPane.getVerticalScrollBar().setUnitIncrement(10);

		final JTabbedPane mainTabbedPane = new JTabbedPane();
		mainTabbedPane
				.addTab("All Steps", null, allStepsScrollPane, Messages.getString("OntologyGenerationComponent.AllStepsTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.BLACK);
		mainTabbedPane
				.addTab("Terms Generation", null, termGenerationScrollPane, Messages.getString("OntologyGenerationComponent.TermGenerationTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.BLACK);
		mainTabbedPane
				.addTab("Definition Generation", null, definitionGenerationScrollPane, Messages.getString("OntologyGenerationComponent.DefinitionGenerationTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.BLACK);
		mainTabbedPane
				.addTab("Preferences", null, preferencesScrollPane, Messages.getString("OntologyGenerationComponent.PreferencesTab.label")); //$NON-NLS-1$//$NON-NLS-2$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.DARK_GRAY);
		mainTabbedPane.addTab("Help", scrollPaneForHelp); //$NON-NLS-1$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.DARK_GRAY);
		mainTabbedPane.addTab("About", aboutPanelForTab); //$NON-NLS-1$
		mainTabbedPane.setForegroundAt(mainTabbedPane.getComponentCount() - 1, Color.DARK_GRAY);

		// init for all panels in one tab
		termGenerationPanelContainer.add(termGenerationPanel);
		definitonGenerationPanelContainer.add(definitionGenerationPanel);
		addToOntologyPanelContainer.add(addToOntologyPanel);

		mainTabbedPane.addChangeListener(new ChangeListener() {

			public void stateChanged(ChangeEvent e) {
				termGenerationPanelContainer.removeAll();
				termGenerationPanelForTab.removeAll();
				definitonGenerationPanelContainer.removeAll();
				definitionGenerationPanelForTab.removeAll();
				addToOntologyPanelContainer.removeAll();
				addToOntologyPanelForTab.removeAll();

				if (mainTabbedPane.getSelectedIndex() == 0) {
					termGenerationPanelContainer.add(termGenerationPanel);
					definitonGenerationPanelContainer.add(definitionGenerationPanel);
					addToOntologyPanelContainer.add(addToOntologyPanel);
					termGenerationPanel.setBorder(titledBorderTermGenerationPanel);
					definitionGenerationPanel.setBorder(titledBorderDefPanel);
					addToOntologyPanel.setBorder(titledBorderAddToOntologyPanel);
				} else {
					termGenerationPanel.setBorder(BorderFactory.createEmptyBorder());
					definitionGenerationPanel.setBorder(BorderFactory.createEmptyBorder());
					addToOntologyPanel.setBorder(BorderFactory.createEmptyBorder());
					termGenerationPanelForTab.add(termGenerationPanel);
					definitionGenerationPanelForTab.add(definitionGenerationPanel);
					addToOntologyPanelForTab.add(addToOntologyPanel);
				}
			}
		});

		JPanel mainTabbedPaneContainer = new JPanel();
		mainTabbedPaneContainer.setLayout(new BoxLayout(mainTabbedPaneContainer, BoxLayout.Y_AXIS));
		mainTabbedPaneContainer.add(mainTabbedPane);

		return mainTabbedPane;
	}

	/**
	 * Displays or hides the progress bar dialog with the default wait text
	 * 
	 * @param displayDlg
	 *            if true, display progress bar dialog, hide otherwise
	 */
	public void showProgressDlg(boolean displayDlg) {
		showProgressDialog(displayDlg, null);
	}

	/**
	 * Displays or hides the progress bar dialog
	 * 
	 * @param displayDlg
	 *            if true, display progress bar dialog, hide otherwise
	 * @param displayMsg
	 *            The wait text inside the dialog. If null, display default wait
	 *            text
	 */
	public void showProgressDialog(boolean displayDlg, String displayMsg) {
		if (displayDlg) {
			if (progressDlg != null) {
				progressDlg.setVisible(false);
			}
			progressDlg = new ProgressBarDialog(guiComponent, displayMsg);
			progressDlg.setVisible(true);
		} else {
			if (progressDlg != null) {
				progressDlg.setVisible(false);
				progressDlg = null;
			}
		}

	}

	public void updateGenerateSiblingsButton() {
		if (!inputSeedTermField1.getText().equals("") && !inputSeedTermField2.getText().equals(""))
			generateSiblingsButton.setEnabled(true);
		else
			generateSiblingsButton.setEnabled(false);
	}

	public void ontologyTermSelectionChanged() {
		OntologyClassInterface selectedOntologyTerm = adapter.getSelectedOntologyTerm();
		if (selectedOntologyTerm == null)
			return;
		List<? extends OntologyClassInterface> children = selectedOntologyTerm.getChildren();
		if (children == null)
			return;
		if (children.size() < 2)
			return;

		// Randomly select at least 2 seed terms
		List<OntologyClassInterface> selectedSeedTerms = new ArrayList<OntologyClassInterface>();
		if (children.size() <= 3) {
			selectedSeedTerms.addAll(children);
		} else {

			Random random = new Random();
			while (selectedSeedTerms.size() < Math.min(children.size(), 3)) {
				int r = random.nextInt(children.size());
				OntologyClassInterface selectedOntologyClass = children.get(r);
				if (!selectedSeedTerms.contains(selectedOntologyClass)) {
					selectedSeedTerms.add(selectedOntologyClass);
				}
			}
		}

		tentativeSeedTerms.clear();
		tentativeSeedTerms.addAll(selectedSeedTerms);

		// Set seed terms in text fields
		inputSeedTermField1.setText(selectedSeedTerms.get(0).getLabel());
		inputSeedTermField2.setText(selectedSeedTerms.get(1).getLabel());
		if (selectedSeedTerms.size() > 2)
			inputSeedTermField3.setText(selectedSeedTerms.get(2).getLabel());
		else
			inputSeedTermField3.setText("");

		generateSiblingsButton.setEnabled(true);
	}

	private class OntologyLookupServiceWorker extends SwingWorker<Void, Void> {
		/**
		 * Constructs an {@link OntologyLookupServiceWorker}
		 */
		public OntologyLookupServiceWorker() {
			super();
			if (ontoLookupProxy == null) {
				// Set ContextClassLoader to load a class inside
				// GoPubMedGenerationStub correctly.
				ClassLoader cl = OntologyLookupManagerPortTypeProxy.class.getClassLoader();
				Thread.currentThread().setContextClassLoader(cl);
				ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
			}
		}

		/**
		 * @return {@link Void}
		 * @throws Exception
		 * @see org.jdesktop.swingworker.SwingWorker#doInBackground()
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected Void doInBackground() throws Exception {
			while (true) {
				try {
					final CandidateTerm candidateTerm = ontologyLookupQueue.take();
					if (candidateTerm.getExistingLookupTerms() == null) {
						final Set<String> lexicalRepresentations = candidateTerm.getLexicalRepresentations();
						String[] labels = new String[lexicalRepresentations.size()];
						labels = lexicalRepresentations.toArray(labels);
						final OBOLookupTerm[] existingOntologyTerms = ontoLookupProxy.lookupExactConcept(null, labels);
						if (existingOntologyTerms == null) {
							candidateTerm.setExistingLookupOntologyTerms(Collections.EMPTY_LIST);
						} else {
							candidateTerm.setExistingLookupOntologyTerms(Arrays.asList(existingOntologyTerms));
							synchronized (OntologyGenerationComponent.class) {
								for (int i = candidateTermsTable.getCurrentFirstVisibleRow(); i <= candidateTermsTable
										.getCurrentLastVisibleRow(); i++) {
									// TODO destroys table witdh (BUG) /
									// synchronize with resize and set terms
									candidateTermsTable.getModel().fireTableCellUpdated(i, 1);
								}
							}
						}
					}
				} catch (InterruptedException exception) {
					logger.info("EBI ontology lookup service has been interrupted.");
				} catch (Throwable exception) {
					logger.error("Ontology lookup service failure!"); //$NON-NLS-1$
				}
			}
		}
	}

	private class BioPortalLookupServiceWorker extends SwingWorker<Void, Void> {
		private static final String BIOPORTAL_APPLICATION_ID = "ac21a542-14db-41ae-a5c3-757f8199c0ea";

		/**
		 * @return {@link Void}
		 * @throws Exception
		 * @see org.jdesktop.swingworker.SwingWorker#doInBackground()
		 */
		@Override
		protected Void doInBackground() throws Exception {
			while (true) {
				try {
					final CandidateTerm candidateTerm = ontologyLookupQueue.take();
					if (candidateTerm.getExistingLookupTerms() == null) {
						final Set<String> lexicalRepresentations = candidateTerm.getLexicalRepresentations();
						String[] labels = new String[lexicalRepresentations.size()];
						labels = lexicalRepresentations.toArray(labels);

						String ONTOLOGY_ID = "";
						String BIOPORTAL_URL_PREFIX = "http://rest.bioontology.org/bioportal/search/";

						String searchParameters = "?ontologyids=" + ONTOLOGY_ID + "&isexactmatch=1"
								+ "&includeproperties=0&apikey=" + BIOPORTAL_APPLICATION_ID;
						// Configure web service URL

						Set<OBOLookupTerm> terms = new HashSet<OBOLookupTerm>();

						for (String label : labels) {

							String bioPortalSearchUrl = BIOPORTAL_URL_PREFIX + label + searchParameters;

							// Call Search REST URL and Parse results
							Collection<OBOLookupTerm> retrievedTerms = ParseXMLFile.parseXMLFile(bioPortalSearchUrl);
							if (retrievedTerms != null)
								terms.addAll(retrievedTerms);
						}

						candidateTerm.setExistingLookupOntologyTerms(new ArrayList<OBOLookupTerm>(terms));
						synchronized (OntologyGenerationComponent.class) {
							for (int i = candidateTermsTable.getCurrentFirstVisibleRow(); i <= candidateTermsTable
									.getCurrentLastVisibleRow(); i++) {
								candidateTermsTable.getModel().fireTableCellUpdated(i, 1);
							}
						}
					}
				} catch (InterruptedException exception) {
					logger.info("BioPortal ontology lookup service has been interrupted.");
				} catch (Throwable exception) {
					logger.error("Ontology lookup service failure!");
				}
			}
		}
	}

	private class OntologyLookupChildrenServiceWorker extends SwingWorker<Void, Void> {
		/**
		 * Constructs an {@link OntologyLookupChildrenServiceWorker}
		 */
		public OntologyLookupChildrenServiceWorker() {
			super();
			if (ontoLookupProxy == null) {
				final ClassLoader cl = OntologyLookupManagerPortTypeProxy.class.getClassLoader();
				Thread.currentThread().setContextClassLoader(cl);
				ontoLookupProxy = new OntologyLookupManagerPortTypeProxy();
			}
		}

		/**
		 * @return {@link Void}
		 * @throws Exception
		 * @see org.jdesktop.swingworker.SwingWorker#doInBackground()
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected Void doInBackground() throws Exception {
			while (true) {
				logger.trace("TAKE: "); //$NON-NLS-1$
				final CandidateTerm candidateTerm = ontologyLookupChildrenQueue.take();
				logger.trace("TAKEN: " + candidateTerm.getLabel()); //$NON-NLS-1$

				try {
					if (candidateTerm.getExistingLookupChildTerms() == null) {
						final Set<String> lexicalRepresentations = candidateTerm.getLexicalRepresentations();
						String[] labels = new String[lexicalRepresentations.size()];
						labels = lexicalRepresentations.toArray(labels);
						// update existing OBO terms
						if (candidateTerm.getExistingLookupTerms() == null) {
							logger.trace("OBO Lookup: "); //$NON-NLS-1$
							final OBOLookupTerm[] existingOntologyTerms = ontoLookupProxy.lookupExactConcept(null,
									labels);
							if (existingOntologyTerms == null) {
								candidateTerm.setExistingLookupOntologyTerms(Collections.EMPTY_LIST);
							} else {
								List<OBOLookupTerm> list = Arrays.asList(existingOntologyTerms);
								candidateTerm.setExistingLookupOntologyTerms(list);
							}
						}
						// update existing OBO child terms
						if (candidateTerm.getExistingLookupChildTerms() == null) {
							Map<String, OBOLookupTerm> allChildrenMap = new HashMap<String, OBOLookupTerm>();
							List<OBOLookupTerm> allChildren = new ArrayList<OBOLookupTerm>();
							List<OBOLookupRelation> allRelations = new ArrayList<OBOLookupRelation>();
							if (candidateTerm.getExistingLookupTerms().size() > 0) {
								for (OBOLookupTerm term : candidateTerm.getExistingLookupTerms()) {
									OBOLookupResult result = ontoLookupProxy.getChildren(term.getOboID(), 1);
									if (result != null) {
										if (result.getTerms() != null) {
											for (OBOLookupTerm childTerm : result.getTerms()) {
												allChildrenMap.put(childTerm.getOboID(), childTerm);
											}
										}
										if (result.getRelations() != null) {
											for (OBOLookupRelation relation : result.getRelations()) {
												allRelations.add(relation);
												OBOLookupTerm childTerm = allChildrenMap.get(relation
														.getOboChildTermID());
												allChildren.add(childTerm);
											}
										}
									}
								}
							}
							candidateTerm.setExistingLookupChildTerms(allChildren);
							candidateTerm.setExistingLookupChildRelations(allRelations);
							// updateSynonymOrChildTable();
						}
					}
				} catch (Throwable exception) {
					logger.error("Ontology children lookup service failure!"); //$NON-NLS-1$
				}
			}
		}
	}

	/**
	 * Inner Class to invoke {@link GoPubMedTermGenerationStub} in a separate
	 * worker thread
	 */
	private class TermGenerationServiceWorker extends SwingWorker<TextConceptRepresentation[], Void> {
		private String inputData = new String();
		private final TermsTable table;
		private final String source;
		private StringBuffer errorMessage;

		/**
		 * Constructs a {@link TermGenerationServiceWorker}
		 * 
		 * @param inputData
		 *            , the query or text uses for the generation
		 * @param destinationTable
		 *            , the table where to place the terms
		 * @param textSourceName
		 *            , the source type (e.g. TEXT,PUBMED)
		 */
		public TermGenerationServiceWorker(String inputData, TermsTable destinationTable, String textSourceName) {
			this.inputData = prepareTextReplaceInvalidCharacter(inputData);
			this.table = destinationTable;
			this.source = textSourceName;
			if (termGenerationServiceStub == null) {
				try {
					// Set ContextClassLoader to load a class inside
					// GoPubMedGenerationStub correctly.
					ClassLoader cl = GoPubMedTermGenerationStub.class.getClassLoader();
					Thread.currentThread().setContextClassLoader(cl);
					termGenerationServiceStub = new GoPubMedTermGenerationStub();
				} catch (AxisFault exception) {
					logger.warn("Term generation initialization failed.", exception);
				}
			} else {
				try {
					termGenerationServiceStub.cleanup();
				} catch (AxisFault exception) {
					try {
						termGenerationServiceStub = new GoPubMedTermGenerationStub();
					} catch (AxisFault exception2) {
						logger.warn("Term generation initialization failed.", exception);
					}
				}
			}

		}

		@Override
		protected TextConceptRepresentation[] doInBackground() {
			this.setProgress(0);
			TextConceptRepresentation[] concepts = new TextConceptRepresentation[1000];

			try {
				ProxyInfo.prepareProxySettings(termGenerationServiceStub);
				if (source.equals(SOURCE_PUBMED)) {
					candidateTermsTable.setShowInformationIcon(true);
					GenerateConceptsFromPubMedQuery query = new GoPubMedTermGenerationStub.GenerateConceptsFromPubMedQuery();
					query.setMaxNumberOfTerms(1000);
					query.setQueryString(inputData);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromPubMedQueryResponse response = termGenerationServiceStub
							.generateConceptsFromPubMedQuery(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_WEB)) {
					candidateTermsTable.setShowInformationIcon(true);
					GenerateConceptsFromWebQueryForLanguage query = new GoPubMedTermGenerationStub.GenerateConceptsFromWebQueryForLanguage();
					query.setLanguage(adapter.getLocale().getLanguage());
					query.setRegion(adapter.getLocale().getCountry().toLowerCase());
					query.setMaxNumberOfTerms(1000);
					query.setQueryString(inputData);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					GenerateConceptsFromWebQueryForLanguageResponse response = termGenerationServiceStub
							.generateConceptsFromWebQueryForLanguage(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_TEXT)) {
					candidateTermsTable.setShowInformationIcon(false);
					String[] lines = inputData.split("\n");
					GenerateConceptsFromTextForLanguage query = new GoPubMedTermGenerationStub.GenerateConceptsFromTextForLanguage();
					query.setMaxNumberOfTerms(1000);
					query.setTexts(lines);
					query.setApplicationCode(PLUGIN_VERSIONED_NAME);
					query.setLanguage(null);
					query.setRegion(null);
					GenerateConceptsFromTextForLanguageResponse response = termGenerationServiceStub
							.generateConceptsFromTextForLanguage(query);
					concepts = response.get_return();
				}
				if (source.equals(SOURCE_FOLDER)) {
					candidateTermsTable.setShowInformationIcon(false);
					File file = new File(inputData);
					String data = "";
					PdfToTextExtraction pdfParser = new PdfToTextExtraction();
					errorMessage = new StringBuffer();
					if (file.isDirectory()) {
						String extractedString = "";
						File[] fileArray = file.listFiles(new FilenameFilter() {
							public boolean accept(File dir, String name) {
								return (!name.startsWith("."));
							}
						});

						for (int i = 0; i < fileArray.length; i++) {
							if (fileArray[i].isFile()) {
								extractedString = pdfParser.fileExtraction(fileArray[i]);
								if (extractedString.equals(PdfToTextExtraction.ERROR)) {
									errorMessage.append(fileArray[i] + "]");
								} else {
									data += extractedString;
								}
							}
						}
					} else if (file.isFile()) {
						data += pdfParser.fileExtraction(file);
						if (data.equals(PdfToTextExtraction.ERROR)) {
							errorMessage.append(file);
							data = "";
						}
					}

					if (!data.equals("")) {
						String[] pdfText = data.split("\n");
						GenerateConceptsFromTextForLanguage query = new GoPubMedTermGenerationStub.GenerateConceptsFromTextForLanguage();
						query.setLanguage(adapter.getLocale().getLanguage());
						query.setRegion(adapter.getLocale().getCountry().toLowerCase());
						query.setMaxNumberOfTerms(1000);
						query.setTexts(pdfText);
						query.setApplicationCode(PLUGIN_VERSIONED_NAME);
						GenerateConceptsFromTextForLanguageResponse response = termGenerationServiceStub
								.generateConceptsFromTextForLanguage(query);
						concepts = response.get_return();
					}
				}
			} catch (RemoteException exception) {
				logger.warn("Term generation failed.", exception);
			} catch (Throwable exception) {
				logger.error("Term generation has failed.", exception);
			} finally {
				ProxyInfo.restoreSystemProxySettings();
			}
			this.setProgress(50);
			return concepts;
		}

		@Override
		public void done() {
			List<CandidateTerm> termsFromService = new ArrayList<CandidateTerm>();
			TextConceptRepresentation[] concepts = new TextConceptRepresentation[1000];
			try {
				concepts = get();
			} catch (InterruptedException ignore) {
				this.setProgress(100);
			} catch (java.util.concurrent.ExecutionException exception) {
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent,
						"Error retrieving terms. Are you connected to the Internet?");
				logger.error("Error retrieving terms!", exception);
			}

			if (source.equals(SOURCE_FOLDER)) {
				String errors = errorMessage.toString();
				if (!errors.equals("")) {
					StringBuffer message = new StringBuffer();
					message.append("Unfortunately, it was not possible to extract information from the given files.\n");
					String files[] = errors.split("]");
					for (int i = 0; i < files.length; ++i) {
						message.append(files[i] + "\n");
					}
					this.setProgress(100);
					JOptionPane.showMessageDialog(guiComponent, message.toString());
				}
			}

			if (concepts == null) {
				// close the progress bar
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent, "No candidate terms found for query '" + inputData + "'.");
			} else {
				for (TextConceptRepresentation concept : concepts) {
					if (concept != null) {
						// set Each OBoTermlabel
						CandidateTerm candidateTerm = null;
						if (candidateTermCache.hasCandidateTermWithLabel(concept.getLabel())) {
							candidateTerm = candidateTermCache.get(concept.getLabel());
							candidateTerm = updateCandidateTermWithConcept(concept, candidateTerm);
						} else if (clipboard.hasCandidateTermWithLabel(concept.getLabel())) {
							candidateTerm = clipboard.get(concept.getLabel());
							candidateTerm = updateCandidateTermWithConcept(concept, candidateTerm);
						} else {
							candidateTerm = new CandidateTerm(concept.getLabel(), concept.getKnownAbbreviation(),
									concept.getLexicalRepresentation(), concept.getScore(),
									CandidateTerm.TYPE_GENERATED);
						}
						termsFromService.add(candidateTerm);
					}
				}
				updateTermsTableHeader(source, inputData);

				this.table.setTerms(termsFromService);

				for (CandidateTerm generatedTerm : termsFromService) {
					if (table.getModel().isInClipboard(generatedTerm)) {
						// add merged existing from clipboard with generated
						table.getModel().addTermToClipboard(generatedTerm);
					}
				}
				// Scroll and select
				JTableHelper.scrollToTopAndSelectFirst(this.table);

				// trigger update ontology lookup
				updateTermsTableUsingOntologyLookup(this.table);

				// finish progessbar
				this.setProgress(100);

				// trigger update ontology lookup
				updateTermsTableUsingOntologyLookup(this.table);
			}
		}

		/**
		 * Update an given candidate term with the information
		 * 
		 * @param concept
		 * @param candidateTerm
		 */
		private CandidateTerm updateCandidateTermWithConcept(TextConceptRepresentation concept,
				CandidateTerm candidateTerm) {
			if (null != concept.getKnownAbbreviation()) {
				for (String abbr : concept.getKnownAbbreviation()) {
					candidateTerm.addAbbreviation(abbr);
				}
			}
			if (null != concept.getLexicalRepresentation()) {
				for (String lex : concept.getLexicalRepresentation()) {
					candidateTerm.addLexicalRepresentation(lex);
				}
			}
			candidateTerm.setScore(Math.max(candidateTerm.getScore(), concept.getScore()));
			return candidateTerm;
		}
	}

	private class SiblingGenerationServiceWorker extends SwingWorker<TextConceptRepresentation[], Void> {
		private static final int requestTimeout = 60000;
		private final TextConceptRepresentation[] seedTerms;
		@SuppressWarnings("unused")
		private final TextConceptRepresentation parentTerm;
		private final TermsTable table;

		private final int maxSiblings = 20;

		public SiblingGenerationServiceWorker(List<String> seedTermStrings, TermsTable destinationTable) {
			seedTerms = new TextConceptRepresentation[seedTermStrings.size()];
			parentTerm = null;
			table = destinationTable;

			int i = 0;
			for (String seedTermString : seedTermStrings) {
				TextConceptRepresentation representation = new TextConceptRepresentation();
				representation.setLabel(seedTermString);
				// String[] abbreviation = {seedTermString};
				// representation.setKnownAbbreviation(abbreviation);
				String[] lexicalRepresentation = { seedTermString };
				representation.setLexicalRepresentation(lexicalRepresentation);

				seedTerms[i] = representation;
				i++;
			}

			createSiblingGenerationStub();
		}

		public SiblingGenerationServiceWorker(List<OntologyClassInterface> seedTerms,
				OntologyClassInterface parentTerm, TermsTable destinationTable) {
			this.seedTerms = new TextConceptRepresentation[seedTerms.size()];
			for (int i = 0; i < seedTerms.size(); i++) {
				this.seedTerms[i] = createConceptRepresentation(seedTerms.get(i));
			}

			this.parentTerm = createConceptRepresentation(parentTerm);
			this.table = destinationTable;

			createSiblingGenerationStub();
		}

		private TextConceptRepresentation createConceptRepresentation(OntologyClassInterface ontologyClass) {
			TextConceptRepresentation representation = new TextConceptRepresentation();

			// LABEL
			representation.setLabel(ontologyClass.getLabel());

			// LEXICAL REPRESENTATIONS
			String[] lexicalRepresentations = null;
			if (ontologyClass.getSynonyms(adapter.getLanguage()) != null
					&& !ontologyClass.getSynonyms(adapter.getLanguage()).isEmpty()) {
				lexicalRepresentations = new String[ontologyClass.getSynonyms(adapter.getLanguage()).size()];
				int j = 0;
				for (String synonym : ontologyClass.getSynonyms(adapter.getLanguage())) {
					lexicalRepresentations[j] = synonym;
					j++;
				}
			} else {
				lexicalRepresentations = new String[1];
				lexicalRepresentations[0] = ontologyClass.getLabel();
			}
			representation.setLexicalRepresentation(lexicalRepresentations);

			// ABBREVIATIONS
			// String[] abbreviations = null;
			// if (ontologyClass.getAbbreviations() != null
			// && !ontologyClass.getAbbreviations().isEmpty()) {
			// abbreviations = new String[ontologyClass.getAbbreviations()
			// .size()];
			// int j = 0;
			// for (String abbreviation : ontologyClass.getAbbreviations()) {
			// abbreviations[j] = abbreviation;
			// j++;
			// }
			// } else {
			// abbreviations = new String[1];
			// abbreviations[0] = ontologyClass.getLabel();
			// }
			// representation.setKnownAbbreviation(abbreviations);

			return representation;
		}

		private void createSiblingGenerationStub() {
			if (siblingGenerationServiceStub == null) {
				try {
					// Set ContextClassLoader to load a class inside
					// GoPubMedGenerationStub correctly.
					ClassLoader cl = GoPubMedTermGenerationStub.class.getClassLoader();
					Thread.currentThread().setContextClassLoader(cl);
					siblingGenerationServiceStub = new GoPubMedTermGenerationStub();
					siblingGenerationServiceStub._getServiceClient().getOptions().setTimeOutInMilliSeconds(60000);
				} catch (AxisFault exception) {
					logger.warn("Term generation initialization failed."); //$NON-NLS-1$
				}
			} else {
				try {
					siblingGenerationServiceStub.cleanup();
				} catch (AxisFault exception) {
					try {
						siblingGenerationServiceStub = new GoPubMedTermGenerationStub();
						siblingGenerationServiceStub._getServiceClient().getOptions()
								.setTimeOutInMilliSeconds(requestTimeout);
					} catch (AxisFault exception2) {
						logger.warn("Term generation initialization failed."); //$NON-NLS-1$
					}
				}
			}

		}

		@Override
		protected TextConceptRepresentation[] doInBackground() throws Exception {
			TextConceptRepresentation[] concepts = null;

			try {
				ProxyInfo.prepareProxySettings(siblingGenerationServiceStub);

				GenerateSiblingsFromWeb query = new GoPubMedTermGenerationStub.GenerateSiblingsFromWeb();
				query.setApplicationCode(PLUGIN_VERSIONED_NAME);
				query.setSeedTerms(seedTerms);
				GenerateSiblingsFromWebResponse response = siblingGenerationServiceStub.generateSiblingsFromWeb(query);
				concepts = response.get_return();
			} catch (RemoteException exception) {
				logger.warn("Sibling generation failed."); //$NON-NLS-1$
			} catch (Throwable exception) {
				logger.error("Sibling generation failed."); //$NON-NLS-1$
			} finally {
				ProxyInfo.restoreSystemProxySettings();
			}

			if (concepts == null)
				return null;

			int siblingCount = Math.min(concepts.length, maxSiblings);

			TextConceptRepresentation[] trimmedConcepts = new TextConceptRepresentation[siblingCount];

			for (int i = 0; i < siblingCount; i++) {
				trimmedConcepts[i] = concepts[i];
			}

			return trimmedConcepts;
		}

		@Override
		public void done() {
			List<CandidateTerm> termsFromService = new ArrayList<CandidateTerm>();
			TextConceptRepresentation[] concepts = new TextConceptRepresentation[1000];
			try {
				concepts = get();
			} catch (InterruptedException ignore) {
				this.setProgress(100);
			} catch (java.util.concurrent.ExecutionException exception) {
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent,
						"Error retrieving terms. Are you connected to the Internet?");
				logger.error("Error retrieving terms!", exception);
			}

			if (concepts == null) {
				// close the progress bar
				this.setProgress(100);
				JOptionPane.showMessageDialog(guiComponent, "No sibling generated.");
			} else {
				for (TextConceptRepresentation concept : concepts) {
					if (concept != null) {
						// set Each OBoTermlabel
						CandidateTerm candidateTerm = null;
						if (candidateTermCache.hasCandidateTermWithLabel(concept.getLabel())) {
							candidateTerm = candidateTermCache.get(concept.getLabel());
							candidateTerm = updateCandidateTermWithConcept(concept, candidateTerm);
						} else if (clipboard.hasCandidateTermWithLabel(concept.getLabel())) {
							candidateTerm = clipboard.get(concept.getLabel());
							candidateTerm = updateCandidateTermWithConcept(concept, candidateTerm);
						} else {
							candidateTerm = new CandidateTerm(concept.getLabel(), concept.getKnownAbbreviation(),
									concept.getLexicalRepresentation(), concept.getScore(),
									CandidateTerm.TYPE_GENERATED_SIBLING);
						}
						termsFromService.add(candidateTerm);
					}
				}
				updateTermsTableHeader(SOURCE_SIBLING, null);

				this.table.setTerms(termsFromService);

				for (CandidateTerm generatedTerm : termsFromService) {
					if (table.getModel().isInClipboard(generatedTerm)) {
						// add merged existing from clipboard with generated
						table.getModel().addTermToClipboard(generatedTerm);
					}
				}
				// Scroll and select
				JTableHelper.scrollToTopAndSelectFirst(this.table);

				// trigger update ontology lookup
				updateTermsTableUsingOntologyLookup(this.table);

				// finish progessbar
				this.setProgress(100);
			}
		}

		/**
		 * Update an given candidate term with the information
		 * 
		 * @param concept
		 * @param candidateTerm
		 */
		private CandidateTerm updateCandidateTermWithConcept(TextConceptRepresentation concept,
				CandidateTerm candidateTerm) {
			if (null != concept.getKnownAbbreviation()) {
				for (String abbr : concept.getKnownAbbreviation()) {
					candidateTerm.addAbbreviation(abbr);
				}
			}
			if (null != concept.getLexicalRepresentation()) {
				for (String lex : concept.getLexicalRepresentation()) {
					candidateTerm.addLexicalRepresentation(lex);
				}
			}
			candidateTerm.setScore(Math.max(candidateTerm.getScore(), concept.getScore()));
			return candidateTerm;
		}
	}

	/**
	 * Worker to invoke the {@link GoPubMedDefinitionGeneratorStub} in a
	 * separate thread
	 */
	private class DefinitionGenerationServiceWorker extends SwingWorker<DefinitionContainer[], Void> {
		private String qTerm = new String();
		private final String[] parents;
		private final DefinitionsTable table;

		/**
		 * Constructs a {@link DefinitionGenerationServiceWorker}
		 * 
		 * @param term
		 *            , the term to be defined
		 * @param relatedTerms
		 *            , known terms associated with the term (e.g. known parent
		 *            terms)
		 */
		public DefinitionGenerationServiceWorker(String term, String[] relatedTerms, DefinitionsTable destinationTable) {
			this.qTerm = prepareTextReplaceInvalidCharacter(term);
			this.parents = relatedTerms;
			this.table = destinationTable;
			if (definitionGeneratorStub == null) {
				try {
					ClassLoader cl = GoPubMedDefinitionGeneratorStub.class.getClassLoader();
					Thread.currentThread().setContextClassLoader(cl);
					definitionGeneratorStub = new GoPubMedDefinitionGeneratorStub();
				} catch (AxisFault exception) {
					logger.error("Definition generation initialization failed."); //$NON-NLS-1$
				}
			} else {
				try {
					definitionGeneratorStub.cleanup();
				} catch (AxisFault exception) {
					try {
						definitionGeneratorStub = new GoPubMedDefinitionGeneratorStub();
					} catch (AxisFault exception2) {
						logger.error("Definition generation initialization failed."); //$NON-NLS-1$
					}
				}
			}
		}

		protected DefinitionContainer[] doInBackground() {
			this.setProgress(0);
			DefinitionContainer[] defs = null;
			try {
				GetDefinitionsForLanguage query = new GoPubMedDefinitionGeneratorStub.GetDefinitionsForLanguage();
				query.setLanguage(adapter.getLocale().getLanguage());
				System.out.println(query.getLanguage());
				query.setRegion(adapter.getLocale().getCountry().toLowerCase());
				System.out.println(query.getRegion());
				String[] termLabels = { qTerm };
				query.setApplicationCode(PLUGIN_VERSIONED_NAME);
				System.out.println(query.getApplicationCode());
				query.setTermLabels(termLabels);
				query.setKnownTerms(parents);
				ProxyInfo.prepareProxySettings(definitionGeneratorStub);
				GetDefinitionsForLanguageResponse response = definitionGeneratorStub.getDefinitionsForLanguage(query);
				defs = response.get_return();
			} catch (Exception exception) {
				logger.error("Definition generation failed."); //$NON-NLS-1$
			} catch (Throwable exception) {
				logger.error("Term generation failed."); //$NON-NLS-1$
			} finally {
				ProxyInfo.restoreSystemProxySettings();
				this.setProgress(50);
			}
			return defs;

		}

		@Override
		public void done() {
			List<CandidateDefinition> defList = new ArrayList<CandidateDefinition>();
			DefinitionContainer[] defs = null;
			try {
				defs = get();
			} catch (InterruptedException ignore) {
				logger.error("Definition Generation interruped."); //$NON-NLS-1$
			} catch (ExecutionException exception) {
				logger.error("Definition Generation failed."); //$NON-NLS-1$
			}
			if (defs != null) {
				int index = 0;

				for (final DefinitionContainer def : defs) {
					organizeDefinition(def, defList, index);
				}
				CandidateTerm termToDefine = selectedCandidateTerm;
				if (termToDefine != null) {
					this.table
							.getColumnModel()
							.getColumn(2)
							.setHeaderValue(
									Messages.getString("OntologyGenerationComponent.DefinitionsForTableColumnHeader") + termToDefine.getLabel() + "\""); //$NON-NLS-1$ //$NON-NLS-2$
					selectedCandidateTerm.setGeneratedDefinitions(defList);
					if (defList.size() > 0) {
						candidateTermCache.addTerm(selectedCandidateTerm);
					}
				} else {
					logger.trace("Invalid selection in termsTable, term is assumed to be selected"); //$NON-NLS-1$
				}
				this.table.setDefinitions(defList);
			} else {
				this.setProgress(100);
				JOptionPane
						.showMessageDialog(
								guiComponent,
								String.format(
										Messages.getString("OntologyGenerationComponent.DefinitionGenerationFailure.text"), selectedCandidateTerm.getLabel())); //$NON-NLS-1$
			}

			// start extension of definitions by parsing from web pages (only
			// check the first 3 incomplete
			int extendCount = 0;
			for (Iterator<CandidateDefinition> iterator = defList.iterator(); iterator.hasNext();) {
				CandidateDefinition candidateDefinition = iterator.next();
				if (candidateDefinition.getDefinition().endsWith("...") //$NON-NLS-1$
						|| !candidateDefinition.getDefinition().endsWith(".")) { //$NON-NLS-1$
					DefinitionExtensionWorker worker = new DefinitionExtensionWorker(candidateDefinition, this.table);
					worker.execute();
					extendCount++;
					if (extendCount % 10 == 0) {
						break;
					}
				}
			}

			this.setProgress(100);

			faviconBitSet.clear();
			updateDefinitionsTableWithFavicons(definitionTable);

			updateParentAsTermFromDefinition(selectedCandidateTerm, candidateTermsTable, ontologyTermsTable,
					definitionTable);
		}
	}

	private class FaviconRetrieverWorker extends SwingWorker<Void, Void> {

		private ExecutorService service;
		private FaviconRetriever retriever;
		private static final int numThreads = 10;

		public FaviconRetrieverWorker() {
			service = Executors.newFixedThreadPool(numThreads);
			retriever = FaviconRetriever.sharedInstance();
		}

		@Override
		protected Void doInBackground() throws Exception {
			while (true) {
				// Blocking call
				final CandidateDefinition definition = faviconRetrievalQueue.take();

				if (definition.isFaviconRetrieved())
					continue;

				Callable<Void> task = new Callable<Void>() {
					public Void call() throws Exception {
						for (final String url : definition.getUrls()) {
							ImageIcon favicon = retriever.getFavicon(url);

							definition.setFaviconRetrieved(true);

							if (retriever.getFavicon(url) != null) {
								definition.setFavicon(favicon);
								definition.setFaviconURL(url);
								definition.setFaviconBaseURL(FaviconRetriever.baseURLFromURL(url));
								// TODO: for some reason, this does not quite
								// work yet. This is why we
								// fireTableDataChanged().
								// definitionTable.getModel().fireTableCellUpdated(definition.getIndex(),
								// 1);
								definitionTable.getModel().fireTableDataChanged();
								break;
							}
						}
						return null;
					}
				};
				service.submit(task);
			}
		}
	}
}
