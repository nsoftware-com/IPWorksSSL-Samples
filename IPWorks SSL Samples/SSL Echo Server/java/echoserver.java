/*
 * IPWorks SSL 2024 Java Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks SSL in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksssl
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

import java.io.*;
import ipworksssl.*;

public class echoserver extends ConsoleDemo {

	private static SSLServer sslserver;

	public static void main(String[] args) {
		if (args.length != 3) {
			System.out.println("usage: echoserver port filename password");
			System.out.println("");
			System.out.println("  port     the TCP port in the local host where the component listens");
			System.out.println("  filename the path to the file containing certificates and optional private keys");
			System.out.println("  password the password for the certificate store file. If the provided test file is used (test.pfx), set the password to \"test\"");
			System.out.println("\r\nExample: echoserver 777 test.pfx test");
		} else {
			try {
				sslserver = new SSLServer();
				System.out.println("*****************************************************************************************");
				System.out.println("* This demo shows how to set up an echo server on your computer.                        *");
				System.out.println("*****************************************************************************************\n");

				sslserver.addSSLServerEventListener(new DefaultSSLServerEventListener() {
					public void SSLClientAuthentication(SSLServerSSLClientAuthenticationEvent arg0) {
						arg0.accept = true;
					}

					public void connected(SSLServerConnectedEvent e) {
						ConnectionMap connections = sslserver.getConnections();
						Connection connection = (Connection) connections.get(e.connectionId);
						System.out.println(connection.getRemoteHost() + " has connected.");
						System.out.print(">");
						try {
							connection.setEOL("\r\n");
						} catch (IPWorksSSLException e1) {
						}
					}

					public void dataIn(SSLServerDataInEvent e) {
						try {
							ConnectionMap connections = sslserver.getConnections();
							Connection connection = (Connection) connections.get(e.connectionId);
							sslserver.sendBytes(connection.getConnectionId(), e.text);
							System.out.println("Echoing '" + new String(e.text) + "' to client " + connection.getRemoteHost() + ".");
							System.out.print(">");
						} catch (IPWorksSSLException e1) {
							e1.printStackTrace();
						}
					}

					public void disconnected(SSLServerDisconnectedEvent e) {
						System.out.println("Disconnected " + e.description + " from " + e.connectionId + ".");
						System.out.print(">");
					}
				});

				sslserver.setSSLCert(new Certificate(4, args[1], args[2], "*"));
				sslserver.setLocalPort(Integer.parseInt(args[0]));
				sslserver.startListening();

				System.out.println("\r\nStarted Listening.");
				System.out.println("\r\nPlease input command: \r\n- 1 Send Data \r\n- 2 Exit");
				System.out.print(">");

				while (true) {
					if (System.in.available() > 0) {
						String command = String.valueOf(read());
						if ("1".equals(command)) {
							String text = prompt("Please input sending data");
							ConnectionMap connections = sslserver.getConnections();
							Object[] keys = connections.keySet().toArray();
							if (keys.length > 0) {
								for (int i = 0; i < keys.length; i++) {
									Connection connection = (Connection) connections.get(keys[i]);
									sslserver.sendText(connection.getConnectionId(), text);
								}
								System.out.println("Sending success.");
							} else {
								System.out.println("\r\nNo connected client.");
							}
							System.out.println("\r\nPlease input command: \r\n- 1 Send Data \r\n- 2 Exit");
							System.out.print(">");
						} else if ("2".equals(command)) {
							break;
						}
					}
				}
				sslserver.stopListening();
				sslserver.shutdown();
				System.out.println(">Stopped Listening.");
			} catch (Exception ex) {
				System.out.println(ex.getMessage());
			}
		}
	}
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksSSLException) {
      System.out.print(" (" + ((IPWorksSSLException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



