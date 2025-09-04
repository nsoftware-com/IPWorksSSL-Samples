/*
 * IPWorks SSL 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworksssl = require("@nsoftware/ipworksssl");

if(!ipworksssl) {
  console.error("Cannot find ipworksssl.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main().catch(e => console.log(e))

async function main () {
  const argv = process.argv;
  if (argv.length !== 5) {
    console.log("Usage: node echoserver.js port filename password");
    console.log("  port	      the TCP port in the local host where the component listens");
    console.log("  filename   the path to the file containing certificates and optional private keys");
    console.log("  password   the password for the certificate store file. If the provided test file is used (test.pfx), set the password to \"test\"");
    console.log("Example: node echoserver.js 777 servercert.pfx test");
    return;
  }
  console.log('*****************************************************************')
  console.log('* This demo shows how to set up an echo server using SSLServer. *')
  console.log('*****************************************************************')

  const sslserver = new ipworksssl.sslserver();

  sslserver.on('Connected', (e) => {
    console.log(sslserver.getConnections().item(e.connectionId).getRemoteHost() + ' connected.')
  }).on('Disconnected', (e) => {
    console.log('Remote host disconnected: ' + e.description)
  }).on('DataIn', async e => {
    console.log("Eachoing '" + e.text + "' back to " + sslserver.getConnections().item(e.connectionId).getRemoteHost() + ".");
    await sslserver.sendText(e.connectionId, e.text).catch(e => console.log(e));
  });

  sslserver.setLocalHost('localhost');
  const cert = new ipworksssl.Certificate(ipworksssl.CertStoreTypes.cstPFXFile, argv[3], argv[4], "*");
  sslserver.setSSLCert(cert);
  sslserver.setLocalPort(parseInt(argv[2]));

  await sslserver.startListening();
  console.log('Listening on port ' + argv[2] + '... press Ctrl-C to shutdown.')

  while (true) {
    await sslserver.doEvents()
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
