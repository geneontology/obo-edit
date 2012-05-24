package org.oboedit.gui.event;

import java.util.*;

public interface VerificationListener extends EventListener {

	public void verificationStarting(VerificationEvent e);

	public void verificationComplete(VerificationEvent e);
}
