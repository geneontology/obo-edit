package org.oboedit.gui.components.ontologyGeneration;

import org.apache.axis2.transport.http.HttpTransportProperties;

public class ProxyInfo
{
	public static final String PROXY_PASSWORD = "http.proxyPassword";
	public static final String PROXY_USERNAME = "http.proxyUser";
	public static final String PROXY_PORT = "http.proxyPort";
	public static final String PROXY_HOST = "http.proxyHost";
	private static String host;
	private static String port;
	private static String username;
	private static String password;

	public static String getHost()
	{
		return host;
	}

	public static void setHost(String host)
	{
		ProxyInfo.host = host;
	}

	public static String getPort()
	{
		return port;
	}

	public static void setPort(String port)
	{
		ProxyInfo.port = port;
	}

	public static String getUsername()
	{
		return username;
	}

	public static void setUsername(String username)
	{
		ProxyInfo.username = username;
	}

	public static String getPassword()
	{
		return password;
	}

	public static void setPassword(String password)
	{
		ProxyInfo.password = password;
	}

	public static void prepareProxySettings(org.apache.axis2.client.Stub stub)
	{

		final String systemHost = System.getProperty(PROXY_HOST);
		if (systemHost != null) {
			setHost(systemHost);
			setPort(System.getProperty(PROXY_PORT));
			setUsername(System.getProperty(PROXY_USERNAME));
			setPassword(System.getProperty(PROXY_PASSWORD));

			HttpTransportProperties.ProxyProperties proxyProperties = new HttpTransportProperties.ProxyProperties();
			if (null != getHost()) {
				proxyProperties.setProxyName(getHost());
				System.getProperties().remove(PROXY_HOST);
				if (null != getPort()) {
					proxyProperties.setProxyPort(Integer.parseInt(getPort()));
					System.getProperties().remove(PROXY_PORT);
				}
				if (null != getUsername() && null != getPassword()) {
					proxyProperties.setUserName(getUsername());
					System.getProperties().remove(PROXY_USERNAME);
					proxyProperties.setPassWord(getPassword());
					System.getProperties().remove(PROXY_PASSWORD);
				}
			}
			// stub._getServiceClient().getOptions().setProperty(org.apache.axis2.transport.http.HTTPConstants.HTTP_PROTOCOL_VERSION,
			// org.apache.axis2.transport.http.HTTPConstants.HEADER_PROTOCOL_10);
			stub._getServiceClient().getOptions().setProperty(org.apache.axis2.transport.http.HTTPConstants.PROXY, proxyProperties);
		}
	}

	public static void restoreSystemProxySettings()
	{
		if (null != getHost())
			System.setProperty(PROXY_HOST, getHost());
		if (null != getPort())
			System.setProperty(PROXY_PORT, getPort());
		if (null != getUsername())
			System.setProperty(PROXY_USERNAME, getUsername());
		if (null != getPassword())
			System.setProperty(PROXY_PASSWORD, getPassword());

	}
}
