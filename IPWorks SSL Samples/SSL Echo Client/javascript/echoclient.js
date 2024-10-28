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

main();
async function main() {
  const argv = process.argv;
  if (argv.length < 4) {
    console.log("Usage: node echoclient.js server port");
    console.log("  server    the address of the remote host.");
    console.log("  port      the TCP port of the remote host");
    console.log("Example: node echoclient.js localhost 777");
    process.exit();
  }

  const sslclient = new ipworksssl.sslclient();

  sslclient.on('DataIn', (e) => {
    console.log("Received '" + e.text + "'");
  }).on('SSLServerAuthentication', function (e) {
    e.accept = true;
  }).on('Disconnected', function (e) {
    console.log("Disconnected " + e.description + " from " + sslclient.getRemoteHost() + ".");
  });

  sslclient.config("AcceptAnyServerCert=true");

  await sslclient.connectTo(argv[2], parseInt(argv[3])).catch((err) => {
    console.log("Error: " + err.message);
    process.exit();
  });

  sslclient.doEvents();
  console.log("Connected.\r\nType and press enter to send. Press Ctrl-C to exit the application.");

  rl.on('line', (line) => {
    sslclient.sendText(line, function (err) {
      if (err) {
        console.log(err);
        process.exit(2);
      }
    });
  });
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
