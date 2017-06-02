haskell-gettext README
======================

This package is pure Haskell implementation of GetText library runtime.
It allows you to:

* Load GNU Gettext binary catalog files (`.mo`, `.gmo`).
* Execute lookups for messages in catalog (gettext and ngettext functions).

Support for plural form selection expressions is fully implemented.

This package is however relatively low-level and may be not very nice to
use in applications. So it can be used as a backend for some more user-friendly
"translation framework".

This package has the following advantages comparing to hgettext:

* It is easier to build it on different platforms, since it has no dependency on 
  C code;
* It does not depend on additional C libraries in runtime;
* And probably the most important: this library does not use global process-level
  variables to store "current catalog" (current locale), the catalog should be
  specified for each call of translation function. So it can be much simpler to
  use this library for example in web applications.

