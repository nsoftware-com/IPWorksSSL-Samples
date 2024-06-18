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

class soapDemo
{
  private static SOAP soap = new nsoftware.IPWorksSSL.SOAP();

  static void Main(string[] args)
  {
    // Process user commands.
    Console.WriteLine("This sample console application converts temperature units using SOAP calls.\nType \"?\" or \"help\" for a list of commands.");
    Console.Write("soap> ");
    string command;
    string[] arguments;

    while (true)
    {
      command = Console.ReadLine();
      arguments = command.Split();

      if (arguments[0] == "?" || arguments[0] == "help")
      {
        Console.WriteLine("Commands:");
        Console.WriteLine(" ?                       display the list of valid commands");
        Console.WriteLine(" help                    display the list of valid commands");
        Console.WriteLine(" f2c <temperature>       convert Fahrenheit value, <temperature>, to Celsius");
        Console.WriteLine(" c2f <temperature>       convert Celsius value, <temperature>, to Fahrenheit");
        Console.WriteLine(" quit                    exit the application");
      }
      else if (arguments[0] == "f2c")
      {
        try
        {
          Console.Write("Converting... ");
          soap.Reset();
          soap.Config("MethodNamespacePrefix=");
          soap.URL = "https://www.w3schools.com/xml/tempconvert.asmx";
          soap.MethodURI = "https://www.w3schools.com/xml/";
          soap.Method = "FahrenheitToCelsius";
          soap.ActionURI = soap.MethodURI + soap.Method;
          soap.AddParam("Fahrenheit", (arguments.Length > 1 ? arguments[1] : "0"));
          soap.SendRequest();
          soap.XPath = "/Envelope/Body/FahrenheitToCelsiusResponse/FahrenheitToCelsiusResult";
          Console.WriteLine(arguments[1] + "F is " + soap.XText + "C");
        }
        catch (Exception ex)
        {
          Console.WriteLine("Could not convert: " + ex.Message);
        }
      }
      else if (arguments[0] == "c2f")
      {
        try
        {
          Console.Write("Converting... ");
          soap.Reset();
          soap.Config("MethodNamespacePrefix=");
          soap.URL = "https://www.w3schools.com/xml/tempconvert.asmx";
          soap.MethodURI = "https://www.w3schools.com/xml/";
          soap.Method = "CelsiusToFahrenheit";
          soap.ActionURI = soap.MethodURI + soap.Method;
          soap.AddParam("Celsius", (arguments.Length > 1 ? arguments[1] : "0"));
          soap.SendRequest();
          soap.XPath = "/Envelope/Body/CelsiusToFahrenheitResponse/CelsiusToFahrenheitResult";
          Console.WriteLine(arguments[1] + "C is " + soap.XText + "F");
        }
        catch (Exception ex)
        {
          Console.WriteLine("Could not convert: " + ex.Message);
        }
      }
      else if (arguments[0] == "")
      {
        // Do nothing.
      }
      else if (arguments[0] == "quit" || arguments[0] == "exit")
      {
        break;
      }
      else
      {
        Console.WriteLine("Invalid command.");
      } // End of command checking.

      Console.Write("soap> ");
    }
  }
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
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
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