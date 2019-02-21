#########################
QuickBooks for Communards
#########################

QuickBooks for Communards is a web application built specifically for
non-Accountant communards, consisting of an Elm frontend and a Haskell API
server that communicates with QuickBooks Desktop using the `QuickBooks
WebConnector`_.

The frontend presents Communards with transaction logs for relevant Accounts as
well as forms for entering their Credit Card purchases, Trip entries, or
Transfers.

This is still in the alpha stage, but under active development.

.. _QuickBooks WebConnector: https://developer.intuit.com/app/developer/qbdesktop/docs/get-started/get-started-with-quickbooks-web-connector


Server
######

We use `Stack`_ to build the server & manage it's dependencies:

.. code:: bash

   cd server
   stack build
   stack exec qbfc-server

Configuration is done through YAML files. Some sensible defaults are loaded
from the ``default-settings.yaml`` file, but this does not contain
instance-specific settings. Therefore, you will need to create a
``settings.yaml`` file in the ``server`` directory, with at least the following
fields:

.. code:: yaml

   account-sync:
      id: "<randomly-generated-uuid>"

Unless you're running the server on the same system as the WebConnector, you'll
need to set the ``hostname`` field as well.

You can generate UUIDs by running the following commands in ``stack repl``::

   :set -XScopedTypeVariables
   import Data.UUID
   import System.Random
   (randomIO :: IO UUID) >>= print

TODO: Add a script for generating minimum ``settings.yaml`` file with
randomized values.


TLS Configuration
=================

The QuickBooks WebConnector requires TLS when connecting to hosts other than
``localhost``. You can use ``openssl`` to generate the self-signed certificate
and key files(if your server is accessible from the internet, you should use a
normal TLS certificate instead):

.. code:: bash

   cd server
   openssl req -x509 -newkey rsa:4096 -sha256 -nodes -days 3650 \
      -subj '/C=US/ST=Virginia/L=Mineral/O=Southern Exposure/CN=<qbfc-server-domain>' \
      -keyout key.pem -out cert.pem

Replace ``<qbfc-server-domain>`` with the FQDN of your QuickBooks for
Communards server(e.g., ``accounting.acorn``). If your server does not have a
domain name, you can use any arbitrary domain, provided you add it to the
``hosts`` file on the computer running QuickBooks. This file usually lives at
``C:\Windows\System32\Drivers\etc\hosts``::

   192.168.1.XXX qbfc-server.local

By default, QuickBooks will not trust a self-signed certificate. You'll need to
tell your QuickBooks computer to trust it:

#. Open Internet Explorer
#. Open the settings menu and select ``Internet Options``
#. Click the ``Content`` tab and then the ``Certificates`` button
#. Click the ``Trusted Root Certification Authorities`` tab and then the
   ``Import`` button
#. Load your generated ``cert.pem`` file
#. Test the certificate by visiting ``https://qbfc-server.local:3000/cert/``,
   you should see a blank page instead of an TLS error or Insecure Webpage
   warning.

.. _Stack: https://docs.haskellstack.org/en/stable/README/


Documentation
#############

Some useful external references:

* `QuickBooks Web Connector Programmer's Guide[PDF]`_
* `QuickBooks Desktop API Reference`_
* `QuickBooks SDK Request/Response Reference`_
* `QBXML v13 Samples`_
* Example SOAP Requests/Responses: `Authenticate`_, `Send Request`_,
  `Receive Response`_, `Close Connection`_
* `ConsoliBYTE's Wiki`_


.. _QuickBooks Web Connector Programmer's Guide[PDF]: https://developer-static.intuit.com/qbSDK-current/doc/PDF/QBWC_ProGuide.pdf
.. _QuickBooks Desktop API Reference: https://developer.intuit.com/app/developer/qbdesktop/docs/api-reference
.. _QuickBooks SDK Request/Response Reference: https://developer-static.intuit.com/qbsdk-current/common/newosr/
.. _QBXML v13 Samples: https://github.com/IntuitDeveloper/QBXML_SDK13_Samples/tree/master/xmlfiles
.. _Authenticate: http://wiki.consolibyte.com/wiki/doku.php/quickbooks_web_connector_soap_authenticate
.. _Send Request: http://wiki.consolibyte.com/wiki/doku.php/quickbooks_web_connector_soap_sendrequestxml
.. _Receive Response: http://wiki.consolibyte.com/wiki/doku.php/quickbooks_web_connector_soap_receiveresponsexml
.. _Close Connection: http://wiki.consolibyte.com/wiki/doku.php/quickbooks_web_connector_soap_closeconnection
.. _ConsoliBYTE's Wiki: http://wiki.consolibyte.com/wiki/doku.php/quickbooks_web_connector


License
#######

GPL-3.0, exceptions possible.
