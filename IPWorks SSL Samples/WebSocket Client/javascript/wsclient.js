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
  if (argv.length < 3) {
    console.log("Usage: node wsclient.js url");
    console.log("  url    the server's URL in the format \"wss://host:[port]/[URI]\"");
    console.log("Example: node echoserver.js wss://echo.websocket.events/.ws");
    console.log("         node echoserver.js wss://localhost:777");
    process.exit();
  }

  const wsclient = new ipworksssl.wsclient();

  wsclient.on('DataIn', (e) => {
    console.log("Received: '" + e.text + "'");
  }).on('SSLServerAuthentication', (e) => {
    e.accept = true;
  });

  var url = argv[2];
  console.log('Connecting to ' + url);

  await wsclient.connectTo(url).catch(e => {
    console.log(e)
    process.exit(2);
  });
  wsclient.doEvents();
  console.log('Connected.\r\nType and press enter to send. Press Ctrl-C to exit the application.');

  rl.on('line', (line) => {
    wsclient.sendText(line, function (err) {
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
