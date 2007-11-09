package org.oboedit.gui;

import java.awt.*;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.text.ParseException;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.plaf.FontUIResource;
import javax.xml.transform.TransformerException;

import java.net.MalformedURLException;
import java.net.URL;

import org.bbop.framework.GUIManager;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.util.VersionNumber;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.widget.TextIcon;
import org.oboedit.util.GUIUtil;

public class Preferences {

	protected Font font;

	protected Color backgroundColor = null;

	protected Color buttonColor = null;

	protected Color selectionColor = null;

	protected Color lightSelectionColor = null;

	protected boolean confirmOnExit = true;

	protected boolean useReasoner = false;

	protected boolean showComplete = false;

	protected boolean useBasicRootDetection = true;

	protected boolean autosaveEnabled = false;

	protected boolean caseSensitiveSort = false;

	protected boolean showToolTips = true;

	protected boolean allowCycles = false;

	protected boolean warnBeforeDelete = true;

	protected boolean warnBeforeDefinitionLoss = true;

	protected boolean allowExtendedCharacters = false;

	protected String mem;

	protected String browserCommand = "";

	protected String userName = System.getProperty("user.name");

	protected String fullName = "";

	protected String email = "";

	protected int autosaveWaitTime = 30;

	protected int autosaveExpirationDays = 1;

	protected int selectionBatchSize = 100;

	protected boolean autoCommitTextEdits = true;

	protected boolean warnBeforeDiscardingEdits = true;

	protected Dbxref personalDbxref;

	protected java.util.List<Dbxref> personalDbxrefs = null;

	protected String defaultDef;

	protected boolean usePersonalDefinition = false;

	// protected Properties iconProperties = new Properties();

	protected MultiProperties adapterChooserProperties = new MultiProperties();

	protected boolean useModalProgressMonitors = !System.getProperty(
			"useModalProgressMonitors", "true").equals("false");

	protected Collection<ReconfigListener> reconfigListeners = new LinkedList<ReconfigListener>();

	public void addReconfigListener(ReconfigListener e) {
		reconfigListeners.add(e);
	}

	public void removeReconfigListener(ReconfigListener e) {
		reconfigListeners.remove(e);
	}

	public void fireReconfigEvent(ReconfigEvent event) {
		Collection<ReconfigListener> temp = new LinkedList<ReconfigListener>(
				reconfigListeners);
		for (ReconfigListener listener : temp) {
			listener.configReloaded(event);
		}
	}

	protected static File installationDir;

	// protected static final File stderrFile = new File(
	// GUIManager.getPrefsDir(), "stderr");
	//
	// protected static final File prefsXMLFile = new File(GUIManager
	// .getPrefsDir(), "config.xml");
	//
	// protected static final File prefsFile = new
	// File(GUIManager.getPrefsDir(),
	// "config");
	//
	// protected File historyFilePath = new File(GUIManager.getPrefsDir(),
	// "history.xml");
	//
	protected File autosavePath;

	protected Map<String, Icon> iconIndex = new HashMap<String, Icon>();
	protected Map<String, String> iconURLIndex = new HashMap<String, String>();

	protected static VersionNumber version;

	protected static final String PLIST_TRANSFORM = "<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">\n"
			+ "  <xsl:output method=\"xml\" indent=\"yes\"/>\n"
			+ "  <xsl:param name=\"memoryOption\" select=\"\'7M\'\"/>\n"
			+ "  <xsl:variable name=\"firstDictID\" select=\"generate-id(/plist/dict[1])\"/>\n"
			+ "  <xsl:template match=\"/\">\n"
			+ "     <xsl:apply-templates/>\n"
			+ "  </xsl:template>\n"
			+ "  <xsl:template match=\"dict\">\n"
			+ "    <xsl:copy>\n"
			+ "      <xsl:copy-of select=\"@*\"/>\n"
			+ "      <xsl:if test=\"generate-id() = $firstDictID and false() = (//key=\'VMOptions\')\">\n"
			+ "	<key>Java</key>\n"
			+ "	<dict>\n"
			+ "	  <key>VMOptions</key>\n"
			+ "	  <string>-Xmx<xsl:value-of select=\"$memoryOption\"/></string>\n"
			+ "	</dict>\n"
			+ "      </xsl:if>\n"
			+ "      <xsl:apply-templates/>\n"
			+ "   </xsl:copy>\n"
			+ "  </xsl:template>\n"
			+ "  <xsl:template match=\"string\">\n"
			+ "   <xsl:copy>\n"
			+ "     <xsl:copy-of select=\"@*\"/>\n"
			+ "     <xsl:choose>\n"
			+ "       <xsl:when test=\"preceding-sibling::*[1] = \'VMOptions\'\">\n"
			+ "         <xsl:text>-Xmx</xsl:text><xsl:value-of select=\"$memoryOption\"/>\n"
			+ "       </xsl:when>\n"
			+ "       <xsl:otherwise>       \n"
			+ "         <xsl:value-of select=\"text()\"/>\n"
			+ "       </xsl:otherwise>\n"
			+ "     </xsl:choose>\n"
			+ "   </xsl:copy>	 	\n"
			+ "  </xsl:template>\n"
			+ "  <xsl:template match=\"node()\">\n"
			+ "    <xsl:copy>\n"
			+ "      <xsl:copy-of select=\"@*\"/>\n"
			+ "      <xsl:apply-templates/>\n"
			+ "    </xsl:copy>\n"
			+ "  </xsl:template>\n" + "</xsl:stylesheet>";

	protected static final File filterFile = new File(GUIManager.getPrefsDir(),
			"filters/");

	protected static Preferences preferences;

	public Preferences() {
		setFont(new Font("Arial", 0, 12));
		iconURLIndex.put(OBOProperty.IS_A.getID(), "resource:isa.gif");
		iconURLIndex.put("part_of", "resource:partof.gif");
		iconURLIndex.put("develops_from", "resource:develops.gif");
	}

	public static Preferences getPreferences() {
		if (preferences == null) {
			XMLDecoder d;
			try {
				d = new XMLDecoder(new BufferedInputStream(new FileInputStream(
						Preferences.getPrefsXMLFile())));
				preferences = (Preferences) d.readObject();
				d.close();
			} catch (Exception e) {
				System.err.println("Could not read preferences file from "
						+ Preferences.getPrefsXMLFile());
				preferences = new Preferences();
			}
			GUIManager.addShutdownHook(new Runnable() {
				public void run() {
					try {
						System.err.println("writing preferences");
						writePreferences(getPreferences());
					} catch (IOException ex) {
						System.err
								.println("Could not write verification settings!");
						ex.printStackTrace();
					}
				}
			});
		}
		return preferences;
	}

	public static File getDictionaryFile() {
		return new File(GUIManager.getPrefsDir(), "dictionary.dict");
	}

	public boolean getAutoCommitTextEdits() {
		return autoCommitTextEdits;
	}

	public void setAutoCommitTextEdits(boolean autoCommitTextEdits) {
		this.autoCommitTextEdits = autoCommitTextEdits;
	}

	public boolean getWarnBeforeDiscardingEdits() {
		return warnBeforeDiscardingEdits;
	}

	public void setWarnBeforeDiscardingEdits(boolean warnBeforeDiscardingEdits) {
		this.warnBeforeDiscardingEdits = warnBeforeDiscardingEdits;
	}

	public void setAllowExtendedCharacters(boolean allowExtendedCharacters) {
		this.allowExtendedCharacters = allowExtendedCharacters;
	}

	public boolean getAllowExtendedCharacters() {
		return allowExtendedCharacters;
	}

	public void setSelectionBatchSize(int selectionBatchSize) {
		this.selectionBatchSize = selectionBatchSize;
	}

	public int getSelectionBatchSize() {
		return selectionBatchSize;
	}

	public void setAdapterChooserProperties(
			MultiProperties adapterChooserProperties) {
		this.adapterChooserProperties = adapterChooserProperties;
	}

	public MultiProperties getAdapterChooserProperties() {
		return adapterChooserProperties;
	}

	public void setWarnBeforeDefinitionLoss(boolean warnBeforeDefinitionLoss) {
		this.warnBeforeDefinitionLoss = warnBeforeDefinitionLoss;
	}

	public boolean getWarnBeforeDefinitionLoss() {
		return warnBeforeDefinitionLoss;
	}

	public void setWarnBeforeDelete(boolean warnBeforeDelete) {
		this.warnBeforeDelete = warnBeforeDelete;
	}

	public boolean getWarnBeforeDelete() {
		return warnBeforeDelete;
	}

	public void setAllowCycles(boolean allowCycles) {
		this.allowCycles = allowCycles;
	}

	public boolean getAllowCycles() {
		return allowCycles;
	}

	public void setShowToolTips(boolean showToolTips) {
		this.showToolTips = showToolTips;
	}

	public boolean getShowToolTips() {
		return showToolTips;
	}

	public void setCaseSensitiveSort(boolean caseSensitiveSort) {
		this.caseSensitiveSort = caseSensitiveSort;
	}

	public boolean getCaseSensitiveSort() {
		return caseSensitiveSort;
	}

	public void setAutosaveExpirationDays(int autosaveExpirationDays) {
		this.autosaveExpirationDays = autosaveExpirationDays;
	}

	public int getAutosaveExpirationDays() {
		return autosaveExpirationDays;
	}

	public void setAutosaveWaitTime(int autosaveWaitTime) {
		this.autosaveWaitTime = autosaveWaitTime;
	}

	public int getAutosaveWaitTime() {
		return autosaveWaitTime;
	}

	public void setAutosaveEnabled(boolean autosaveEnabled) {
		this.autosaveEnabled = autosaveEnabled;
	}

	public boolean getAutosaveEnabled() {
		return autosaveEnabled;
	}

	public File getAutosavePath() {
		if (autosavePath == null)
			autosavePath = new File(GUIManager.getPrefsDir(), "autosave");
		return autosavePath;
	}

	public void setAutosavePath(File autosavePath) {
		this.autosavePath = autosavePath;
	}

	public Dbxref getPersonalDbxref() {
		if (personalDbxref != null)
			return personalDbxref;
		else
			return new DbxrefImpl("", getUserName(), Dbxref.UNKNOWN);
	}

	public void setPersonalDbxref(Dbxref personalDbxref) {
		this.personalDbxref = personalDbxref;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getEmail() {
		return email;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getFullName() {
		return fullName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getUserName() {
		return userName;
	}

	public void setBrowserCommand(String browserCommand) {
		this.browserCommand = browserCommand;
	}

	public String getBrowserCommand() {
		return browserCommand;
	}

	public void setUseBasicRootDetection(boolean useBasicRootDetection) {
		this.useBasicRootDetection = useBasicRootDetection;
	}

	public boolean getUseBasicRootDetection() {
		return useBasicRootDetection;
	}

	protected static ClassLoader getExtensionLoader() {
		return Preferences.class.getClassLoader();
	}

	public static File[] getExtensionPaths() {
		File[] out = { new File(getInstallationDirectory(), "extensions"),
				new File(GUIManager.getPrefsDir(), "extensions") };
		return out;
	}

	protected Icon loadLibraryIconLocal(String name) {
		URL url = getExtensionLoader().getResource(
				"org/oboedit/gui/resources/icons/" + name);

		return new ImageIcon(url);
	}

	public static Image loadLibraryImage(String name) {
		URL url = getExtensionLoader().getResource(
				"org/oboedit/gui/resources/icons/" + name);
		return Toolkit.getDefaultToolkit().createImage(url);
	}

	public static String readMemStringFromDisk() {
		File optionFile = new File(getInstallationDirectory(), Preferences
				.getLauncherName()
				+ ".vmoptions");
		String mem = null;
		if (optionFile.exists()) {
			try {
				BufferedReader reader = new BufferedReader(new FileReader(
						optionFile));
				String line;
				while ((line = reader.readLine()) != null) {
					String s = line.toLowerCase().trim();
					if (line.toLowerCase().trim().startsWith("-xmx")) {
						mem = s.substring(4).toUpperCase();
						reader.close();
						break;
					}
				}
				reader.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
		if (mem == null)
			mem = "512M";
		return mem;
	}

	public String getMemString() {
		if (mem == null) {
			mem = readMemStringFromDisk();
		}
		return mem;
	}

	public void setMemoryValue(String mem) {
		this.mem = mem;
	}

	public boolean getUseReasoner() {
		return useReasoner;
	}

	public void setUseReasoner(boolean useReasoner) {
		this.useReasoner = useReasoner;
	}

	public boolean getConfirmOnExit() {
		return confirmOnExit;
	}

	public void setConfirmOnExit(boolean confirmOnExit) {
		this.confirmOnExit = confirmOnExit;
		GUIManager.setConfirmOnExit(confirmOnExit);
	}

	public Color getLightSelectionColor() {
		if (lightSelectionColor == null)
			lightSelectionColor = new Color(230, 230, 255);
		return lightSelectionColor;
	}

	public void setLightSelectionColor(Color lightSelectionColor) {
		this.lightSelectionColor = lightSelectionColor;
	}

	public Color getSelectionColor() {
		if (selectionColor == null)
			selectionColor = new Color(204, 204, 255);
		return selectionColor;
	}

	public void setSelectionColor(Color selectionColor) {
		this.selectionColor = selectionColor;
	}

	public void setButtonColor(Color buttonColor) {
		this.buttonColor = buttonColor;
	}

	public Color getButtonColor() {
		if (buttonColor == null)
			buttonColor = new Color(100, 149, 237);
		return buttonColor;
	}

	public Color getBackgroundColor() {
		if (backgroundColor == null)
			backgroundColor = new Color(216, 223, 230);
		return backgroundColor;
	}

	public void setBackgroundColor(Color backgroundColor) {
		this.backgroundColor = backgroundColor;
	}

	public java.util.List<Dbxref> getPersonalDbxrefs() {
		return personalDbxrefs;
	}

	public void setPersonalDbxrefs(java.util.List personalDbxrefs) {
		this.personalDbxrefs = personalDbxrefs;
	}

	public String getPersonalDefinition() {
		return "<autodef>";
	}

	public void setPersonalDefinition(String defaultDef) {
		this.defaultDef = defaultDef;
	}

	public boolean getUsePersonalDefinition() {
		return usePersonalDefinition;
	}

	public void setUsePersonalDefinition(boolean usePersonalDefinition) {
		this.usePersonalDefinition = usePersonalDefinition;
	}

	public Font getFont() {
		return font;
	}

	public void setFont(Font font) {
		this.font = font;
		FontUIResource resource = new FontUIResource(font);
		Hashtable defaults = UIManager.getDefaults();
		Enumeration keys = defaults.keys();
		while (keys.hasMoreElements()) {
			Object o = keys.nextElement();
			if (o instanceof String) {
				String key = (String) o;
				if (!key.toLowerCase().contains("menu")
						&& key.toLowerCase().endsWith("font")) {
					defaults.put(key, resource);
				}
			}
		}
		for (Frame f : Frame.getFrames()) {
			SwingUtilities.updateComponentTreeUI(f);
		}
	}

	public void updateInstallJLaunchers() throws IOException {
		File optionFile = new File(getInstallationDirectory(), Preferences
				.getLauncherName()
				+ ".vmoptions");
		PrintWriter stream = new PrintWriter(new FileWriter(optionFile));
		stream.println("-Xmx" + getMemString());
		stream.close();
	}

	public void updateInfoPlist() throws IOException {
		String mem = getMemString();
		if (mem == null)
			return;
		File infoPlist = new File(getInstallationDirectory(), Preferences
				.getLauncherName()
				+ ".app/Contents/Info.plist");
		Map params = new HashMap();
		params.put("memoryOption", mem);
		try {
			XMLUtil.transform(Preferences.PLIST_TRANSFORM, infoPlist, params);
		} catch (TransformerException ex) {
			ex.printStackTrace();
		}
	}

	public void updateLauncherConfigurations() {
		if (GUIUtil.isMacOS()) {
			try {
				updateInfoPlist();
			} catch (IOException ex) {
				System.err.println("Could not update launcher script");
			}
		}
		try {
			updateInstallJLaunchers();
		} catch (IOException ex) {
			System.err.println("Could not update InstallJ "
					+ "launcher scripts");
		}
	}

	public static String getLauncherName() {
		return System.getProperty("launcherName", "oboedit");
	}

	public static List<URL> getIconLibrary() {
		List<URL> iconLib = new LinkedList<URL>();
		try {
			InputStream stream = getExtensionLoader().getResourceAsStream(
					"org/oboedit/gui/resources/" + "icons/dir");
			Properties icons = new Properties();
			icons.load(stream);
			int size = Integer.parseInt(icons.getProperty("iconCount"));
			for (int i = 0; i < size; i++) {
				String iconName = icons.getProperty("icon" + i);
				URL iconURL = getExtensionLoader().getResource(iconName);
				iconLib.add(iconURL);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return iconLib;
	}

	protected static void writePreferences(Preferences preferences)
			throws IOException {
		XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(
				new FileOutputStream(getPrefsXMLFile())));
		encoder.writeObject(preferences);
		encoder.close();
		preferences.updateLauncherConfigurations();
	}

	protected static void fillInInstallationDirectory() {
		// read the installation directory from a system property, if possible
		String prop = System.getProperty("launcherDir");
		if (prop != null) {
			installationDir = new File(prop);
			if (installationDir.exists() && installationDir.isDirectory()) {
				return;
			}
		}

		// if not, try to figure it out from the classpath (not pretty)
		StringTokenizer tokenizer = new StringTokenizer(System
				.getProperty("java.class.path"), System
				.getProperty("path.separator"));
		while (tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();
			File file = new File(token);
			if (file.getName().equals("oboedit.jar")) {
				try {
					installationDir = file.getCanonicalFile().getParentFile()
							.getParentFile();
					return;
				} catch (IOException e) {
					e.printStackTrace();
				}
			} else if (file.getName().equals("oboedit_launcher.jar")) {
				try {
					installationDir = file.getCanonicalFile().getParentFile();
					return;
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

	public static VersionNumber getVersion() {
		if (version == null) {
			try {
				URL url = getExtensionLoader().getResource(
						"org/oboedit/resources/VERSION");
				BufferedReader reader = new BufferedReader(
						new InputStreamReader(url.openStream()));
				version = new VersionNumber(reader.readLine());
				reader.close();
			} catch (Exception e) {
				try {
					version = new VersionNumber("0.0");
				} catch (ParseException e1) {
					e1.printStackTrace();
				}
			}
		}
		return version;
	}

	public static File getInstallationDirectory() {
		if (installationDir == null)
			fillInInstallationDirectory();
		return installationDir;
	}

	public static File getStderrFile() {
		return new File(GUIManager.getPrefsDir(), "stderr");
	}

	public static File getPrefsXMLFile() {
		return new File(GUIManager.getPrefsDir(), "config.xml");
	}

	public static File getPrefsFile() {
		return new File(GUIManager.getPrefsDir(), "config");
	}

	public static File getFiltersDir() {
		return filterFile;
	}

	public static Icon loadLibraryIcon(String name) {
		return getPreferences().loadLibraryIconLocal(name);
	}

	public static URL getLibraryIconURL(String name) {
		URL url = getExtensionLoader().getResource(
				"org/oboedit/gui/resources/icons/" + name);
		return url;
	}

	public static Color lightSelectionColor() {
		return getPreferences().getLightSelectionColor();
	}

	public static Color defaultSelectionColor() {
		return getPreferences().getSelectionColor();
	}

	public static Color defaultBackgroundColor() {
		return getPreferences().getBackgroundColor();
	}

	public static Color defaultButtonColor() {
		return new Color(100, 149, 237);
	}

	public boolean getUseModalProgressMonitors() {
		return useModalProgressMonitors;
	}

	public void setUseModalProgressMonitors(boolean useModalProgressMonitors) {
		this.useModalProgressMonitors = useModalProgressMonitors;
	}

	/*
	 * private void installFilteredRenderers() { int count = 0; MultiProperties
	 * props = config.getProperties("filteredRenderers");
	 * 
	 * try { String countStr = props.getProperty("count"); if (countStr != null)
	 * count = Integer.parseInt(countStr); } catch (NumberFormatException e) { }
	 * for(int i=0; i < count; i++) { MultiProperties rprops =
	 * props.getProperties("filteredRenderer"+i); String classname = (String)
	 * rprops.get("classname"); MultiProperties subrprops = (MultiProperties)
	 * rprops. getProperties("props"); if (classname != null) { try { Class
	 * rClass = extensionLoader.loadClass(classname); FilteredRenderer renderer =
	 * (FilteredRenderer) rClass.newInstance(); //
	 * renderer.setProperties(subrprops); addGlobalFilteredRenderer(renderer); }
	 * catch (Exception e) { System.err.println("couldn't load "+classname+"
	 * because of "+e); } } } }
	 */

	public void setIconURLIndex(Map<String, String> map) {
		this.iconURLIndex = map;
		iconIndex.clear();
	}

	public Map<String, String> getIconURLIndex() {
		return iconURLIndex;
	}

	public Icon getIconForRelationshipType(OBOProperty type) {
		Icon out = (Icon) iconIndex.get(type.getID());
		if (out == null) {
			String iconURL = iconURLIndex.get(type.getID());
			if (iconURL != null) {
				if (iconURL.startsWith("resource:")) {
					out = loadLibraryIcon(iconURL.substring(9));
				} else {
					try {
						URL url = new URL(iconURL);
						out = new ImageIcon(url);
					} catch (MalformedURLException ex) {
					}
				}
			}
			if (out == null) {
				out = new TextIcon(type.getName());
			}
			iconIndex.put(type.getID(), out);
		}
		return out;
	}
}
