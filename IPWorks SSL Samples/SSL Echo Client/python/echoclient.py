# 
# IPWorks SSL 2024 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks SSL in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworksssl
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworksssl import *

input = sys.hexversion < 0x03000000 and raw_input or input


def ensureArg(args, prompt, index):
    if len(args) <= index:
        while len(args) <= index:
            args.append(None)
        args[index] = input(prompt)
    elif args[index] is None:
        args[index] = input(prompt)


dataReceived = False

def fireError(e):
  print("ERROR: " %e.description)

def fireSSLServerAuthentication(e):
  e.accept = True

def fireDataIn(e):
  global dataReceived
  dataReceived = True
  print ("Received the data: " + bytes.decode(e.text))

print("*****************************************************************")
print("* This is a demo to show how to connect to a remote echo server *")
print("* to send data and receive the echoed response.                 *")
print("*****************************************************************")

dataReceived = False

try:
  sslclient = SSLClient()
  sslclient.set_timeout(10)
  sslclient.on_data_in = fireDataIn
  sslclient.on_ssl_server_authentication = fireSSLServerAuthentication

  server = ""
  server = input("Remote Host (localhost): ")
  if server == "":
    server = "localhost"

  port = ""
  port = input("Remote Port (777): ")
  if port == "":
    port = "777"

  print("Connecting to " + server + ":" + port + "...")
  sslclient.connect_to(server, int(port))
  while True:
    command = int(input("Please input command: 1 [Send Data] or  2 [Exit]: "))
    if command == 1:
      send = input("Please enter data to send: ")
      sslclient.send(bytes(send, 'utf-8'))
      dataReceived = False
      print("Now waiting for response...")
      while dataReceived == False:
        sslclient.do_events()
    else:
      sslclient.disconnect()
      break
    
except IPWorksSSLError as e:
  print("ERROR: %s" % e.message)




