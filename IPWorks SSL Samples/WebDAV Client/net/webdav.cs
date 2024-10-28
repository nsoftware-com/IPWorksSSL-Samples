/*
 * IPWorks SSL 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using nsoftware.IPWorksSSL;

class webdavDemo
{
  private static WebDAV webdav = new nsoftware.IPWorksSSL.WebDAV();

  static void Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: webdav /u username /p password");
      Console.WriteLine("  username   the username to login");
      Console.WriteLine("  password   the password to login");
      Console.WriteLine("\r\nExample: webdav /u username /p password");
    }
    else
    {
      webdav.OnConnected += webdav_OnConnected;
      webdav.OnConnectionStatus += webdav_OnConnectionStatus;
      webdav.OnDisconnected += webdav_OnDisconnected;
      webdav.OnSSLServerAuthentication += webdav_OnSSLServerAuthentication;
      webdav.OnTransfer += webdav_OnTransfer;
      webdav.OnDirList += webdav_OnDirList;

      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        // Parse arguments into component.
        webdav.User = myArgs["u"];
        webdav.Password = myArgs["p"];

        // Process user commands.
        Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
        Console.Write("webdav> ");
        string command;
        string[] arguments;

        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                                      display the list of valid commands");
            Console.WriteLine("  help                                   display the list of valid commands");
            Console.WriteLine("  ls <resource uri>                      list the contents of the specified directory");
            Console.WriteLine("  make <resource uri>                    make a new directory at the specified location (ex. make https://localhost:443/directoryName)");
            Console.WriteLine("  move <source uri> <destination uri>    move a specified resource to a new location (ex. move https://localhost:443/oldFolder/file.txt https://localhost:443/newFolder/file.txt)");
            Console.WriteLine("  get <resource uri>                     get a specified resource");
            Console.WriteLine("  delete <resource uri>                  delete a specified resource");
            Console.WriteLine("  put <local file> <resource uri>        send data to the server");
            Console.WriteLine("  quit                                   exit the application");
          }
          else if (arguments[0] == "ls")
          {
            if (arguments.Length > 1) webdav.ListDirectory(arguments[1]);
          }
          else if (arguments[0] == "make")
          {
            if (arguments.Length > 1) webdav.MakeDirectory(arguments[1]);
          }
          else if (arguments[0] == "move")
          {
            if (arguments.Length > 2) webdav.MoveResource(arguments[1], arguments[2]);
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1) webdav.GetResource(arguments[1]);
          }
          else if (arguments[0] == "delete")
          {
            if (arguments.Length > 1) webdav.DeleteResource(arguments[1]);
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              webdav.LocalFile = arguments[1];
              webdav.PutResource(arguments[2]);
            }
          }
          else if (arguments[0] == "quit" || arguments[0] == "exit")
          {
            break;
          }
          else if (arguments[0] == "")
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          } // End of command checking.

          Console.Write("webdav> ");
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
    }
  }

  #region "Events"

  private static void webdav_OnSSLServerAuthentication(object sender, WebDAVSSLServerAuthenticationEventArgs e)
  {
    // This will trust all certificates and is not recommended for production use.
    e.Accept = true;
  }

  private static void webdav_OnConnected(object sender, WebDAVConnectedEventArgs e)
  {
    Console.WriteLine("Server connected");
  }

  private static void webdav_OnConnectionStatus(object sender, WebDAVConnectionStatusEventArgs e)
  {
    Console.WriteLine("Status code " + e.StatusCode + ": " + e.Description);
  }

  private static void webdav_OnDisconnected(object sender, WebDAVDisconnectedEventArgs e)
  {
    Console.WriteLine("Server disconnected");
  }

  private static void webdav_OnTransfer(object sender, WebDAVTransferEventArgs e)
  {
    Console.WriteLine("Resource being received from server (in full text): \n" +
                                    "========================================= \n" + e.Text);
  }

  private static void webdav_OnDirList(object sender, WebDAVDirListEventArgs e)
  {
    Console.WriteLine(e.DisplayName + ": " + e.ResourceURI);
  }

  #endregion
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}