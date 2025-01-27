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


def fireError(e):
  print("ERROR: " %e.message)

def fireSSLServerAuthentication(e):
  e.accept = True

def fireDataIn(e):
  global dataReceived
  dataReceived = True
  print ("Receive the data: " + bytes.decode(e.text))

print("*****************************************************************")
print("* This is a demo to show how to connect to a web socket echo    *")
print("* server to send data, and receive the echoed response.         *")
print("*****************************************************************")

dataReceived = False
url = input("Echo Server (wss://localhost:777): ")
if url == "":
  url = "wss://localhost:777"

try:
  websocketclient = WSClient()
  websocketclient.set_timeout(10)
  websocketclient.on_data_in = fireDataIn
  websocketclient.on_ssl_server_authentication = fireSSLServerAuthentication

  print(url)
  websocketclient.connect_to(url)
  while True: 
    command = int(input("Please input command: 1 [Send Data] or  2 [Exit]: "))
    if command == 1:
      send = input("Please enter data to send: ")
      websocketclient.send_text(send)
      dataReceived = False
      print("Now waiting for response...")
      while dataReceived == False:
        websocketclient.do_events()
    else:
      websocketclient.disconnect()
      break
      
except IPWorksSSLError as e:
  print("ERROR: " + e.message)


