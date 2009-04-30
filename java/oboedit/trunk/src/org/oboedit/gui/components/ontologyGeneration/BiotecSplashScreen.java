package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URL;
import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.apache.log4j.Logger;
import org.bbop.swing.BackgroundImagePanel;

/**
 * SplashScreen for OboEdit
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), Sep 25, 2008
 */
public class BiotecSplashScreen extends JDialog
{

	private static final long serialVersionUID = 6993432775310354679L;
	protected final static Logger logger = Logger.getLogger(BiotecSplashScreen.class);

	private JPanel bip;
	private final static String biotecSplashScreen = "resources/ontogenSplashScreen.png";

	public BiotecSplashScreen(final Component parent)
	{
		super();
		setAlwaysOnTop(true);
		setModal(true);
		bip = getSplashPanel();
		getContentPane().add(bip);
		getContentPane().setBackground(Color.white);
		setUndecorated(true);
		setSize(500, 400);
		setFocusable(true);

		// center
		Point parentAnchor = parent.getLocation();
		SwingUtilities.convertPointToScreen(parentAnchor, parent);
		int x = parentAnchor.x + (parent.getWidth() - this.getWidth()) / 2;
		int y = parentAnchor.y + (parent.getHeight() - this.getHeight()) / 2;

		setLocation(x, y);

		bip.addKeyListener(new KeyAdapter()
		{
			@Override
			public void keyPressed(KeyEvent e)
			{
				dispose();
			}
		});

		bip.addFocusListener(new FocusAdapter()
		{
			// TODO does not work
			@Override
			public void focusLost(FocusEvent e)
			{
				dispose();
			}
		});

		bip.addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent e)
			{
				dispose();
			}
		});
	}

	public static JPanel getSplashPanel()
	{
		JLabel textLabel = new JLabel()
		{
			private static final long serialVersionUID = -4472416435803158030L;

			@Override
			public void paint(Graphics g)
			{
				Graphics2D g2 = (Graphics2D) g;
				g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
				super.paint(g);
			}
		};

		URL url = BiotecSplashScreen.class.getResource(biotecSplashScreen);
		logger.debug("load image from url: '" + url.getPath() + "'");

		BackgroundImagePanel backgroundImagePanel = new BackgroundImagePanel(url, false);
		backgroundImagePanel.setLayout(null);
		backgroundImagePanel.add(textLabel);
		backgroundImagePanel.setBackground(Color.WHITE);

		GregorianCalendar calendar = new GregorianCalendar();
		int year;
		year = calendar.get(Calendar.YEAR);
		if (year < 2007)
			year = 2007;

		// This is limited by the area in the splashscreen gif

		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("<html>");
		stringBuffer
		    .append("<h3><b>GoPubMed Ontology Generation plugin</b> for OBOEdit 2<small>, v " + OntologyGenerationComponent.PLUGIN_VERSION + "a</small></h3>");
		stringBuffer.append("<p>The plugin was created by <b>Thomas W&auml;chter</b>.</p>");
		stringBuffer.append("<p>Special thanks to: <b>Atif Iqbal</b>, <b>G&ouml;tz Fabian</b>, <b>Marcel Hanke</b>.</p>");
		stringBuffer.append("<p><i>Under the terms of the Artistic License, TU Dresden, (c)2007-" + year + "</i></p>");
		stringBuffer.append("<br>");
		stringBuffer.append("<p>");
		stringBuffer.append("The plugin uses an ontology generation and a definition extraction web services developed at TU Dresden. Both services are hosted at TU Dresden.");
		stringBuffer.append("</p>");
		stringBuffer.append("<br>");
		stringBuffer.append("<p>");
		stringBuffer.append("For commercial use of these services, please contact <b>info@transinsight.com</b>.");
		stringBuffer.append("</p>");
		stringBuffer.append("<h5>Contact</h5>");
		stringBuffer.append("<small>");
		stringBuffer.append("Thomas W&auml;chter, Dipl.-Inf.");
		stringBuffer.append("<br>");
		stringBuffer.append("Bioinformatics Group (BIOTEC), TU Dresden");
		stringBuffer.append("<br>");
		stringBuffer.append("Tatzberg 47-51");
		stringBuffer.append("<br>");
		stringBuffer.append("01307 Dresden, Germany");
		stringBuffer.append("<br>");
		stringBuffer.append("email: waechter(at)biotec.tu-dresden.de");
		stringBuffer.append("</small>");
		stringBuffer.append("</html>");
		textLabel.setBounds(20, 110, 460, 300);
		textLabel.setFont(new Font("Helvetica", Font.PLAIN, 12));
		textLabel.setForeground(Color.DARK_GRAY);
		textLabel.setOpaque(false);
		textLabel.setText(stringBuffer.toString());

		backgroundImagePanel.setBorder(BorderFactory.createEtchedBorder());
		backgroundImagePanel.setSize(new Dimension(500, 400));
		backgroundImagePanel.setPreferredSize(new Dimension(500, 400));
		backgroundImagePanel.setMinimumSize(new Dimension(500, 400));
		return backgroundImagePanel;
	}
}
