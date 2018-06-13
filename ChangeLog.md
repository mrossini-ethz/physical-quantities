# Change log

Physical quantities uses [semantic versioning](http://semver.org/).

## Version 0.1.1 - 2018-06-13:

### Features

  * Added a function for conversion to base units.

### Bugfixes

  * Fixed the printing of units like `mm / m` which were not printed before.
  * Fixed the printing of reducible units like `m s / s`.
  * Fixed the numeric operations that require unitless values. The correct conversion factors (e.g. `1/1000` for `mm / m`) are now applied.

## Version 0.1.0 - 2017-06-13:

Initial release
