package org.oboedit.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import javax.swing.JOptionPane;

import org.oboedit.gui.Preferences;

import org.apache.log4j.Logger;

/** If user is starting a new version of OBO-Edit (and they've previously run an older one),
    warn that they may need to move their oboedit_config/perspectives (because stale binary
    files in it can cause problems), and offer to move it for them. */

public class WarnIfNewVersion {
    protected final static Logger logger = Logger.getLogger(WarnIfNewVersion.class);

    public static void warn(String configDir) {
        String perspectives = configDir + "/perspectives";
        String warnFilename = perspectives + "/version.txt";
        File warnFile = new File(warnFilename);
        if (!warnFile.exists()) {
            logger.debug(warnFilename + " didn't exist"); // DEL
            offerToRenamePerspectives(configDir, perspectives, "older_version");
            createWarnFile(warnFilename);
            return;
        }

        try {
            String version = Preferences.getVersion().toString();
            BufferedReader reader = new BufferedReader(new FileReader(warnFile));
            String line = reader.readLine();
            reader.close();
            if (line == null) {
                logger.debug(warnFilename + " is empty!");
                createWarnFile(warnFilename);
                return;
            }
            String oldVersion = line.trim();
            // We've already seen this version--no need to do anything.
            if (oldVersion.equals(version)) {
                logger.debug("User has run " + version + " before."); // DEL
                return;
            }
            logger.debug("Version changed! Before: " + oldVersion + "; after = " + version);
            offerToRenamePerspectives(configDir, perspectives, oldVersion);
            createWarnFile(warnFilename);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    private static void createWarnFile(String warn) {
        String version = Preferences.getVersion().toString();
        try {
            Writer out = new OutputStreamWriter(new FileOutputStream(warn));
            out.write(version);
            out.close();
            logger.debug("Saved version string to " + warn + ": " + version);
        } catch (Exception e) {
            logger.debug("Failed to write to warn file " + warn + ": " + e.getMessage());
        }
    }

    private static void offerToRenamePerspectives(String configDir, String perspectives, String oldVersion) {
        String message = "I see you have switched to a new version of OBO-Edit.\nPlease note that OBO-Edit saves binary information in its configuration directory\n(" + configDir + ") that is version-specific, which can cause unexpected problems\n(for example, in the Text Editor) when you run a different version of OBO-Edit.\n\nFor best results, you should remove or rename the directory " + perspectives + "\nand then quit and relaunch OBO-Edit (but be aware that doing this will restore the\narrangement of components in the OBO-Edit window to their default settings.)\n\nIf you'd like, I can rename your perspectives directory for you now.\nRename perspectives?";
        if (!(JOptionPane.showConfirmDialog(null, message, "Version update warning", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION))
            return;

        String renamed = perspectives + "_" + oldVersion;
        logger.debug("Renaming " + perspectives + " to " + renamed);
        File pers = new File(perspectives);
        File re = new File(renamed);
        // If the renamed directory already exists, remove it first, or if that's not possible, try to rename it.
        if (re.exists()) {
            if (!re.delete()) {
                message = "Failed to delete existing directory " + re + " so that\n" + perspectives + " could be moved to " + re + "\nProbably a permissions problem.";
                logger.info(message);
                String renamed2 = perspectives + "_" + oldVersion + "_1";
                File re2 = new File(renamed2);
                if (re2.exists()) {
                    message += "\nAnd I also couldn't rename " + re + "\nto " + re2 + ".\nYou'll have to remove " + perspectives + " yourself if you want to get rid of it.";
                    logger.info(message);
                    JOptionPane.showMessageDialog(null, message, "Couldn't rename perspectives", JOptionPane.WARNING_MESSAGE);
                    return;
                }
                if (!re.renameTo(re2)) {
                    logger.info("Failed to rename " + re + " to " + re2);
                    JOptionPane.showMessageDialog(null, message, "Couldn't rename perspectives", JOptionPane.WARNING_MESSAGE);
                    return;
                }
            }
        }      
        if (!pers.renameTo(re)) {
            message = "Failed to rename " + perspectives + " to " + re + "\nProbably a permissions problem.";
            JOptionPane.showMessageDialog(null, message, "Couldn't rename perspectives", JOptionPane.WARNING_MESSAGE);
            return;
        }
        // Now we need to create a new perspectives directory so that version.txt can be saved to it
        pers.mkdir();
    }
}