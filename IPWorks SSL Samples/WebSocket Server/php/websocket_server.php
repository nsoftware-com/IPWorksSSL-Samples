<?php
/*
 * IPWorks SSL 2024 PHP Edition - Sample Project
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
require_once('../include/ipworksssl_wsserver.php');
require_once('../include/ipworksssl_certmgr.php');
require_once('../include/ipworksssl_const.php');
?>
<?php
global $wsserver;
class MyWSServer extends IPWorksSSL_WSServer
{
  function FireConnected($param) {
    global $wsserver;
    echo $wsserver->getWSConnectionRemoteHost($param['connectionid']) . "\n";
  }
  function FireDisconnected($param) {
    echo "Remote host disconnected: " . $param['description'] . "\n";
  }
  function FireDataIn($param) {
    global $wsserver;
    echo "Received '" . $param['text'] . "' from " . $wsserver->getWSConnectionRemoteHost($param['connectionid']) . "\n";
    echo "Echoing message back...\n";
    $wsserver->doSendText($param['connectionid'], $param['text']);
  }
  function FireSSLClientAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
}

$wsserver = new MyWSServer();
if ($argc < 4) {
  echo "Usage: php websocket_server.php port certFile certPassword\n\n";
  echo "  port:          the port on which the server will listen\n"; 
  echo "  certFile:      the certificate file to use for SSL\n";
  echo "  certPassword:  the password for the certificate file\n\n";
  echo "Example: php websocket_client.php 777 test.pfx test\n";
  return;
} else {
  echo "*****************************************************************\n";
  echo "* This demo shows how to set up an echo server using WSServer.  *\n";
  echo "*****************************************************************\n";
  $wsserver->setLocalPort($argv[1]);
  $wsserver->setSSLCertStoreType(99); // auto
  $wsserver->setSSLCertStore($argv[2]);
  $wsserver->setSSLCertStorePassword($argv[3]);
  $wsserver->setSSLCertSubject("*");
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

try {
  $wsserver->doStartListening();
  echo "Listening on port " . $wsserver->getLocalPort() . "... press Ctrl-C to shutdown.\n";

  while (true) {
    $wsserver->doEvents();
  }
} catch (Exception $ex) {
  echo 'Error: ',  $ex->getMessage(), "\n";
}
?>