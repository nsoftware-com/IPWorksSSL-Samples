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

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] is None:
    args[index] = input(prompt)


def fireConnected(e):
  global sslserver
  print(sslserver.get_remote_host(e.connection_id) + " has connected.")

def fireDisconnected(e):
  global sslserver
  print("Disconnected from %s" % sslserver.get_remote_host(e.connection_id))

def fireDataIn(e):
  global sslserver
  print("Received a message from %s and the message is: %s" % (sslserver.get_remote_host(e.connection_id), e.text.decode("utf-8")))
  print("Echoing the message back...")
  sslserver.send(e.connection_id, e.text)

def fireSSLClientAuthentication(e):
  e.accept = True

print("*****************************************************************\n")
print("* This demo shows how to setup an echo server using SSLServer.  *\n")
print("*****************************************************************\n")


buffer = ""
buffer = input("Local Port (777): ")
if (buffer == ""):
  buffer = "777"
port = int(buffer)

try:
  sslserver = SSLServer()
  sslserver.on_connected = fireConnected
  sslserver.on_disconnected = fireDisconnected
  sslserver.on_data_in = fireDataIn
  sslserver.on_ssl_client_authentication = fireSSLClientAuthentication

  buffer = ""
  while buffer == "":
    print("Please choose a certificate type:")
    print("1: PFX File")
    print("2: PEMKey File")
    buffer = input("Selection: ")

  if buffer == "1" or buffer == "": #by default use PFX
    sslserver.set_ssl_cert_store_type(2)
  else:
    sslserver.set_ssl_cert_store_type(6)

  buffer = ""
  buffer = input ("Enter Certificate File Location (.\\test.pfx): ")
  if (buffer == ""):
    buffer = ".\\test.pfx"

  sslserver.set_ssl_cert_store(buffer)

  buffer = ""
  buffer = input("Enter Certificate Password (test): ")
  if buffer == "":
    buffer = "test"

  sslserver.set_ssl_cert_store_password(buffer)
  sslserver.set_ssl_cert_subject("*")

  sslserver.set_local_port(port)
  sslserver.set_local_host("localhost")
  sslserver.start_listening()
  print("Listening...")
  print("Press Ctrl-C to shutdown")
  while True:
    sslserver.do_events()

except IPWorksSSLError as e:
  print(e)
  print("ERROR: %s" % e.message)

except KeyboardInterrupt:
  print("Shutdown requested... exiting.")
  sslserver.shutdown()


