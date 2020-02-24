#######################
QuickBooks for Communes
#######################

.. image:: https://travis-ci.org/prikhi/quickbooks-for-communes.svg?branch=master
    :target: https://travis-ci.org/prikhi/quickbooks-for-communes
    :alt: QuickBooks for Communes Build Status


QuickBooks for Communes is a web application built specifically for
non-Accountant communards, consisting of a Purescript frontend and a Haskell
API server that communicates with QuickBooks Desktop using the `QuickBooks
WebConnector`_.

This is a port of our `Acorn Accounting`_ application and still in the alpha
stage.

The frontend will present Communards with transaction logs for relevant
Accounts as well as forms for entering their Credit Card purchases, Town Trips,
or Internal Transfers.

.. _QuickBooks WebConnector: https://developer.intuit.com/app/developer/qbdesktop/docs/get-started/get-started-with-quickbooks-web-connector
.. _Acorn Accounting: https://github.com/prikhi/acornaccounting



Goals
#####

Eventually we'd like to have support for:

* Forms for communards to account for their Trips, Credit Card Purchases, &
  Internal Transfers.
* A workflow for accountants to approve entries before they are pushed to
  QuickBooks.
* Public views of approved entries & their receipts.
* A Chart of Accounts designed for communards, along with detailed account
  ledgers.
* Support for attaching receipts to entries.
* User accounts for communards, allowing them go back and edit their
  non-approved entries.
* Tracking of communards membership dates, used to pay monthly & yearly
  stipends.


We don't plan on:

* Supporting multiple languages.
* Supporting multiple currencies.
* Managing existing QuickBooks data. We only want to push data *to* QuickBooks.



Build
#####

``manage.hs`` is a management script for the project. You can use it to make
production builds, rebuild the code on file changes, & clean any build
artifacts.

You need `Stack`_ & `Yarn`_ installed. Then you can simply run ``./manage.hs``
to start the development servers. ``./manage.hs build`` will make production
builds for the server & client, while ``./manage.hs clean`` will remove the
built files.

You can also do some lower-level building of the frontend & backend.

.. _Stack: https://docs.haskellstack.org/en/stable/README/
.. _Yarn: https://yarnpkg.com/


Client
======

We use `Spago`_ & `pscid`_ to build the client code & `Parcel`_ to bundle &
serve the UI during development.

.. code:: bash

   cd client
   yarn install
   yarn run watch
   yarn run serve

The ``watch`` command rebuilds the client when the source files change, and
``serve`` starts a development server that builds the styles, serves the index,
& redirects requests to the ``/api/`` path to the backend server.

To assemble the code, styles, & HTML for production, run ``yarn run build``.

.. _Spago: https://github.com/spacchetti/spago
.. _pscid: https://github.com/kRITZCREEK/pscid
.. _Parcel: https://parceljs.org/


Server
======

We use `Stack`_ to build the server & manage it's dependencies, with a
``Makefile`` for running common commands :

.. code:: bash

   cd server
   make run
   make watch



Configuration
#############

Server configuration is done through YAML files. Some sensible defaults are
loaded from the ``default-settings.yaml`` file, but this does not contain
instance-specific settings. Therefore, you will need to create a
``settings.yaml`` file in the ``server`` directory, with at least the following
fields:

.. code:: yaml

   account-sync:
      id: "<some-uuid>"

Unless you're running the server on the same system as the WebConnector, you'll
need to set the ``hostname`` field as well.

You can generate UUIDs by running the following commands in ``stack repl``::

   :set -XScopedTypeVariables
   import Data.UUID
   import System.Random
   (randomIO :: IO UUID) >>= print

TODO: Add a script for generating minimum ``settings.yaml`` file with
randomized values? Generate on first run if one doesn't exist?


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


Documentation
#############

You can build the client's documentation with ``yarn``::

   cd client
   yarn run docs

The documentation will be placed in ``/client/generated-docs/index.html``.


You can build the server's package documentation & open it in your web
browser::

   cd server
   make docs


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
