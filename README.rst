
About
=====

This work is part of the mOSAIC project open-source outcomes released under the Apache 2.0 license (see the "Notice" section below).
    http://developers.mosaic-cloud.eu/

...


How to build
============

**It should be built** from the ``mosaic-distribution`` repository, as it depends on a proper set workbench and symlinks to other
repositories...

* dependencies:

 * ``erlang`` version ``R15Bx`` (where ``x`` is ``01``, ...);
 * ``gcc`` version ``4.x``;
 * ``vbs`` and ``ninja`` latest version;
 * correct setup of symlinks towards other source code repositories;

* inside a console, into the root folder of the project: ::

  ./scripts/prepare
  ./scripts/compile

How to run
==========

* inside a console, into the root folder of the project: ::

  ./scripts/run-node


How to use
==========

Point your browser at http://127.0.155.0:31808/ and you'll get access to an administrative console.

For a better user interface see the ``mosaic-node-wui`` repository.


Notice
======

This product includes software developed at "Institute e-Austria, Timisoara",
as part of the "mOSAIC -- Open-Source API and Platform for Multiple Clouds"
research project (an EC FP7-ICT Grant, agreement 256910).

* http://developers.mosaic-cloud.eu/
* http://www.ieat.ro/

Developers:

* Ciprian Dorin Craciun, ciprian@volution.ro / ciprian.craciun@gmail.com

Copyright: ::

   Copyright 2010-2011, Institute e-Austria, Timisoara, Romania
       http://www.ieat.ro/
   
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at:
       http://www.apache.org/licenses/LICENSE-2.0
   
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

