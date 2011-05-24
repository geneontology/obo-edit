package org.oboedit.gui.components.ontologyGeneration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.apache.axis2.transport.http.HttpTransportProperties;
import org.apache.commons.codec.binary.Base64;
import org.apache.log4j.Logger;

/**
 * Handles http proxy settings. Gets initialized first with system settings and
 * second JVM settings. If authentication is required, user and password is
 * asked.
 * 
 * @author waechter
 */
public class ProxyInfo {
	public static final String PROXY_PASSWORD = "http.proxyPassword";
	public static final String PROXY_USERNAME = "http.proxyUser";
	public static final String PROXY_PORT = "http.proxyPort";
	public static final String PROXY_HOST = "http.proxyHost";
	private static String host;
	private static String port;
	private static String username;
	private static String password;
	private static String _host;
	private static String _port;
	private static String _username;
	private static String _password;
	private static Logger logger = Logger.getLogger(ProxyInfo.class);
	private static ChangeListener listener;
	private static boolean initialized = false;

	public static String getHost() {
		return host;
	}

	public static void setHost(String host) {
		ProxyInfo.host = host;
	}

	public static String getPort() {
		return port;
	}

	public static void setPort(String port) {
		ProxyInfo.port = port;
	}

	public static String getUsername() {
		return username;
	}

	public static void setUsername(String username) {
		ProxyInfo.username = username;
	}

	public static String getPassword() {
		return password;
	}

	public static void setPassword(String password) {
		ProxyInfo.password = password;
	}

	public static void prepareProxySettings(org.apache.axis2.client.Stub stub) {

		initializeSettings();
		if (null != host && null != port && !host.isEmpty() && !port.isEmpty()) {
			testConnection();
		}

		// neccessary for axis2, is required to be reset late
		System.getProperties().remove(PROXY_HOST);
		System.getProperties().remove(PROXY_PORT);
		System.getProperties().remove(PROXY_USERNAME);
		System.getProperties().remove(PROXY_PASSWORD);

		if (host != null) {
			HttpTransportProperties.ProxyProperties proxyProperties = new HttpTransportProperties.ProxyProperties();
			if (null != host) {
				proxyProperties.setProxyName(host);
				if (null != port) {
					proxyProperties.setProxyPort(Integer.parseInt(port));
				}
				if (null != username && null != password) {
					proxyProperties.setUserName(username);
					proxyProperties.setPassWord(password);
				}
			}
			boolean b = null != username && null != password;
			logger.debug("Use Axis2 with proxy " + host + ":" + port + " with username/password=" + String.valueOf(b));

			stub._getServiceClient().getOptions()
					.setProperty(org.apache.axis2.transport.http.HTTPConstants.PROXY, proxyProperties);
		}
	}

	public static void resetToSystemProxySettings() {
		if (null != _host)
			System.setProperty(PROXY_HOST, _host);
		if (null != _port)
			System.setProperty(PROXY_PORT, _port);
		if (null != _username)
			System.setProperty(PROXY_USERNAME, _username);
		if (null != _password)
			System.setProperty(PROXY_PASSWORD, _password);
		host = _host;
		port = _port;
		username = _username;
		password = _password;

		listener.stateChanged(new ChangeEvent(ProxyInfo.class));
	}

	public static void restoreSystemProxySettings() {
		if (null != host)
			System.setProperty(PROXY_HOST, host);
		if (null != port)
			System.setProperty(PROXY_PORT, port);
		if (null != username)
			System.setProperty(PROXY_USERNAME, username);
		if (null != password)
			System.setProperty(PROXY_PASSWORD, password);
		listener.stateChanged(new ChangeEvent(ProxyInfo.class));
	}
	
	public static void initializeSettings() {
		if (initialized == true) {
			return;
		}
		if (null != System.getProperty(PROXY_HOST)) {
			/*
			 * CASE 1: properties set with
			 * -Dhttp.proxyHost=projects.biotec.tu-dresden.de
			 * -Dhttp.proxyPort=3129 -Dhttp.proxyUser=<USER>
			 * -Dhttp.proxyPassword=<PASSWORD>
			 */
			if (null == host)
				host = System.getProperty(PROXY_HOST);
			if (null == port)
				port = System.getProperty(PROXY_PORT);
			if (null == username)
				username = System.getProperty(PROXY_USERNAME);
			if (null == password)
				password = System.getProperty(PROXY_PASSWORD);

			boolean b = null != username && null != password;
			logger.debug("Initialized proxy from JVM arguments. proxy=" + host + ":" + port
					+ " with username/password=" + String.valueOf(b));
		} else {
			/* CASE 2: properties set in system settings, get proxy object */
			System.setProperty("java.net.useSystemProxies", "true");
			List<Proxy> l = null;
			try {
				l = ProxySelector.getDefault().select(new URI("http://www.yahoo.com"));
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}

			if (l != null) {
				for (Iterator<Proxy> iter = l.iterator(); iter.hasNext();) {
					Proxy proxy = iter.next();
					logger.trace("proxy type : " + proxy.type());
					InetSocketAddress addr = (InetSocketAddress) proxy.address();
					if (addr == null) {
						logger.trace("No Proxy");
					} else {
						logger.debug("proxy hostname : " + addr.getHostName() + ":" + addr.getPort());
						if (null == host)
							ProxyInfo.setHost(addr.getHostName());
						if (null == port)
							ProxyInfo.setPort(String.valueOf(addr.getPort()));
						boolean b = null != username && null != password;
						logger.debug("Initialized proxy from System settings. proxy=" + host + ":" + port
								+ String.valueOf(b));
					}
				}
			}
		}
		_host = host;
		_port = port;
		_username = username;
		_password = password;
		initialized = true;
		listener.stateChanged(new ChangeEvent(ProxyInfo.class));
	}

	private static void testConnection() {
		URL url;
		try {
			Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(host, Integer.valueOf(port)));
			url = new URL("http://www.yahoo.com");
			HttpURLConnection uc = (HttpURLConnection) url.openConnection(proxy);
			String encoded = new String(Base64.encodeBase64(new String(username + ":" + password).getBytes()));
			uc.setRequestProperty("Proxy-Authorization", "Basic " + encoded);
			uc.connect();
			
			if (uc.getResponseCode() == HttpURLConnection.HTTP_PROXY_AUTH) {
				requestAuthenticationCredentials();
			}
		} catch (MalformedURLException exception) {
			showConnectionErrorDialog(exception.getLocalizedMessage());
		} catch (IOException exception) {
			showConnectionErrorDialog(exception.getLocalizedMessage());
		}
	}

	private static void requestAuthenticationCredentials() {
		JPanel credentialsPanel = new JPanel(new BorderLayout());
		
		JTextField userNameField;
		JPasswordField passwordField;
		
		JPanel labelPanel = new JPanel(new GridLayout(2, 1));
		JPanel fieldPanel = new JPanel(new GridLayout(2, 1));

		// USERNAME
		{
			JLabel label = new JLabel("Username:", JLabel.RIGHT);
			userNameField = new JTextField(10);
			label.setLabelFor(userNameField);
			
			JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			panel.add(userNameField);
			
			labelPanel.add(label);
			fieldPanel.add(panel);
		}
		// PASSWORD
		{
			JLabel label = new JLabel("Password:", JLabel.RIGHT);
			passwordField = new JPasswordField(10);
			label.setLabelFor(passwordField);
			passwordField.setEchoChar('*');
			
			JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			panel.add(passwordField);
			
			labelPanel.add(label);
			fieldPanel.add(panel);
		}
		
		credentialsPanel.add(labelPanel, BorderLayout.WEST);
		credentialsPanel.add(fieldPanel, BorderLayout.CENTER);
		
		JOptionPane.showMessageDialog(null, credentialsPanel, "Enter proxy user and password", JOptionPane.OK_OPTION);

		setUsername(userNameField.getText());
		setPassword(new String(passwordField.getPassword()));
		
		listener.stateChanged(new ChangeEvent(ProxyInfo.class));
	}
	
	private static void showConnectionErrorDialog(String message) {
		JOptionPane.showMessageDialog(null, message, "Connection error", JOptionPane.ERROR_MESSAGE);
	}

	public static void registerListener(ChangeListener listener) {
		ProxyInfo.listener = listener;
		ProxyInfo.initializeSettings();
	}
}
