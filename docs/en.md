---
title: Vocabulary Server
published: true
sidemenu: true
description: Documentation for the installation and use of the vocabulary server for connection to Goobi
---

## About this manual

This document describes the new vocabulary server. Until version 24.06, vocabularies were part of Goobi Workflow and saved in Goobi's database. Since version 24.07, everything related to vocabularies moved to a stand-alone application, the vocabulary server. The vocabulary server requires its own database to store all its data and provides access to the vocabularies and records through a REST API. Goobi Workflow has been updated to use the new vocabulary server instead of its own, embedded vocabularies. If desired, the vocabulary server could be publicly available. If you already used vocabularies before, check out the migration guide in this documentation to transfer your data to the new vocabulary server.

The source code of UGH can be found on GitHub.

[https://github.com/intranda/goobi-vocabulary-server](https://github.com/intranda/goobi-vocabulary-server)


## Contact

If you have any questions about this documentation, suggestions for the further development of this manual or general questions about Goobi, digitisation projects in general and, of course, the further development of Goobi, please do not hesitate to contact intranda GmbH:

| Contact |  |
| :--- | :--- |
| Address: | intranda GmbH, Bertha-von-Suttner Str. 9, D-37085 Göttingen |
| Phone: | +49 551 291 76 100 |
| Email: | [info@intranda.com](mailto:info@intranda.com) |
| URL: | [https://www.intranda.com](https://www.intranda.com) |

## Copyright

Please note that this documentation may not be modified or passed on in modified form. Commercial use of this documentation is not permitted.

:::info
![copyright](icon_cc.png) 

This work is licensed under the Creative Commons Attribution-Non Commercial-No Derivatives 4.0 International License. To view a copy of this license, visit [http://creativecommons.org/licenses/by-ncnd/4.0/](http://creativecommons.org/licenses/by-ncnd/4.0/) or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
:::
