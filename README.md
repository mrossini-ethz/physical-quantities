# Physical Quantities

This lisp library handles physical quantities which consist of

 * Value/Magnitude
 * Uncertainty/Error
 * Unit

where the type of the value can be any subtype of `real`. For the uncertainty, both absolute and relative values are possible. Combinations of lisp symbols are used to describe units. Unit abbreviations and prefixes as well as user defined units are supported. Error propagation and unit checking is performed when doing calculations.

## Example usage
The following example illustrates the use of the library:
```
(require :asdf)
(asdf:load-system :physical-quantities)
(use-package :physical-quantities}
(define-si-units)
(define-read-macro)

(let (m c E)
  ;; Define the mass
  (setf m #q(1.00 +/- 0.01 kg))
  ;; The speed of light
  (setf c #q(299792458 m / s))
  ;; E = m * c^2
  (setf E (q* m (qpow c 2)))
  ;; Print E, converted to petajoule
  (print #q(E -> PJ)))
```
This will print
```
#<QUANTITY VALUE: 89.87552, ERROR: 1.0 %, UNIT: PETAJOULE>
```
## Defining quantities
To define a quantity, either the macro `(quantity ...)` or the read macro `#q(...)` can be used. For the latter, the function `(define-read-macro)` must be called first.

The following lines of code are equivalent:
```
(quantity 1 kilogram)
#q(1 kilogram)
#q(1 kg)
```
This creates an object of type `quantity`. Its value is the integer `1` and its unit is `KILOGRAM`. The uncertainty/error of this quantity is zero because it was not specified. To do this, the symbol `+/-` (or `+-`), followed by a number, can be inserted into the macro call after the value:
```
#q(1 +/- 0.2 kg)
```
This sets the absolute error to 0.2 kg. To define a relative error, the `%` sign has to be appended:
```
#q(1 +/- 20 % kg)
```
Note that there must be space between numbers and symbols.

It is permitted to use variables or even lisp forms instead of numbers for both the value/magnitude or the uncertainty/error.

### Specifying units
Units are specified as a sequence of unit factors. A unit factor is essentially a unit and a power. The power defaults to 1 when only naming the unit (e.g. `metre`). If a power of -1 is desired, either the symbol `/` or `per` can be inserted before the unit (e.g `/ metre` or `per metre`). Powers other than 1 or -1 are specified in any of the following ways:
```
metre ^ 2
metre ** 2
metre to the 2
/ metre ^ 2
/ metre ** 2
/ metre to the 2
```
Note that `/ metre` is equivalent to `metre ^ -1`.

As pointed out above, the full unit is a sequence of such unit factors:
```
kilogram metre ^ 2 / second ^ 2 / kelvin / mol
```
Please note that separating symbols with spaces is compulsory.

## Operations
Common Lisp does not allow the redefinition of standard operators such as `+` or `*`. For this reason, a number of operators are provided to work with both types `number` and `quantity`. These are prefixed with the letter `q`, e.g. `q+` or `q*`. Example:
```
(q* a (q+ b (qsqrt c)))
```

When using these operations, error propagation will be performed automatically.  The error propagation is first-order only and correlations are not taken into account.

## Converting units
Units can be converted by calling the `(quantity ...)` or `#q(...)` macro and specifying `->` and a new unit:
```
(let ((v #q(20 m / s)))
  (setf v #q(v -> km / h))
  (print v))
```
This would print
```
#<QUANTITY VALUE: 72, ERROR: 0, UNIT: KILOMETRE / HOUR>
```
Instead of using a variable as the first form in the macro call, one could specify any other form such as
```
#q((q* 1/2 m (qpow v 2)) -> joule)
```
or even a quantity definition
```
#q(20 +/- 1 % m / s -> km / h)
```
Note that it is an error if the units are not convertible.

## Accessing value, error and unit
To retrieve value/magnitude, uncertainty/error and unit of an object of type `quantity`, one can use the functions

* `(value <quantity>)` to retrieve the value/magnitude
* `(unit <quantity>)` to retrieve the unit
* `(absolute-error <quantity>)` or `(aerr <quantity>)` to retrieve the absolute uncertainty/error
* `(relative-error <quantity>)` or `(rerr <quantity>` to retrieve the relative uncertainty/error

These are all places, so they are `setf`able. Note that setting the absolute uncertainty will affect the relative uncertainty and vice versa. Also note that it is an error accessing the relative error when the value/magnitude is zero.

## Defining new units
New units can be defined using the `(define-units ...)` macro. Each enclosed form defines a new unit:
```
(define-units
  (metre :abbrev m :alias meter :prefix-max 3)
  (watthour :def (1 watt hour) :abbrev wh)
  (mile :def (1609344/1000 metre) :abbrev mi)
  (gravity :def (981/100 metre / second ^ 2 :abbrev g))
```
Note that there may be naming conflicts. For all defined units and abbreviations more units with all prefixes will be automatically defined such as `KILOWATTHOUR` or `KWH`. Especially for abbreviations, this may cause conflicts. It is possible to limit the number of admissible/automatic prefixes (see below).

There are a number of keywords that may appear in the definition:

* `:def` Specifies the unit in terms of another unit. Complex dependencies can be given such as (0.3048 metre / second ^ 2). If this keyword is missing, the newly defined unit will be considered a base unit.
* `:alias` Allows multiple (non-abbreviated) names for a unit. This is helpful if there are different spellings (e.g. meter, metre).
* `:abbrev` An abbreviation for the unit can be specified here. Be careful about name conflicts (see above).
* `:prefix-max` Specifies the largest admissible prefix. This is useful if some prefixes are never used for a unit (e.g. megametre) or to avoid name conflicts.
* `:prefix-min` Specifies the smallest admissible prefix. This is useful if some prefixes are never used for a unit (e.g. millimile) or to avoid name conflicts.
* `:base` Specifies the admissible bases for the unit prefixes and defaults to 10. Example where a different basis is used: kibibyte = 1024 bytes. Therefore the unit `byte` allows both base 10 and base 1024.

## Defining prefixes
Prefixes can be defined using the `(define-unit-prefixes ...)` macro:
```
(define-unit-prefixes
  (giga g 9)
  (tera t 12)
  (gibi gi 3 :base 1024)
  (tibi ti 4 :base 1024))
```
The full name is specified first, followed by an abbreviation and the power. The default base for the power is 10. In this example, `giga` is equivalent to 10^9.

## Standard units and prefixes
When calling the function `(define-si-units)`, the SI units will be automatically defined. More sets may be available in the future.

## License

Physical Quantities: Lisp Library

Copyright (C) 2017 Marco Rossini

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
