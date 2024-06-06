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
require_once('../include/ipworksssl_http.php');
require_once('../include/ipworksssl_const.php');
?>
<?php
class MyHTTP extends IPWorksSSL_HTTP
{
  function fireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
}

if ($argc < 2) {
  echo "Usage: php get_url.php url\n\n";
  echo "  url: the url to fetch\n\n";
  echo "Example: php get_url.php https://www.nsoftware.com\n";
  return;
} else {
  $url = $argv[1];
}

$http = new MyHTTP();

try {
  $http->setFollowRedirects(1);
  $http->setTransferredDataLimit(0);
  $http->doGet($url);
  echo "Contents of " . $url . ":\n";
  echo $http->getTransferredData();
} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>
