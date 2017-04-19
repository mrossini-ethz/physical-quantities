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

### Unit abbreviations
Units can be abbreviated. This means that `kilometre` is interpreted in the same way as `km`. Note that both the unit `metre` and the prefix `kilo` is abbreviated. Mixing abbreviation (e.g. `kmetre` or `kilom`) is forbidden.

While unit abbreviations are allowed, their use is somewhat dangerous. Mostly, the problem is that lisp does not distinguish between lower- and uppercase letters when reading symbols, converting everything to uppercase. This leads to name conflicts and/or confusion:

* Is `MM` millimetre or megametre?
* Is 'EV' electronvolt or exavolt?
* Is 'PA' pascal or picoampere?

If you want to make sure that your probe does not crash on mars, it is better to avoid using abbreviations.

The problem can be avoided by defining units and prefixes using the `|...|` syntax, e.g. `|Pa|`. One would have to use this syntax throughout the code using the units, however.

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

### Limitations
The unit conversion only considers the conversion factor. This means that linear conversions with offset (not to speak of nonlinear conversions) may not work as expected. For example
```
#q(1 celsius -> kelvin)
```
will produce 1 kelvin. The offset of 273.15 is ignored. This is fine for conversion of temperature differences, but not for absolute temperatures.

## Accessing value, error and unit
To retrieve value/magnitude, uncertainty/error and unit of an object of type `quantity`, one can use the functions

* `(value <quantity>)` to retrieve the value/magnitude
* `(unit <quantity>)` to retrieve the unit
* `(absolute-error <quantity>)` or `(aerr <quantity>)` to retrieve the absolute uncertainty/error
* `(relative-error <quantity>)` or `(rerr <quantity>` to retrieve the relative uncertainty/error

These are all places, so they are `setf`able. Note that setting the absolute uncertainty will affect the relative uncertainty and vice versa. Also note that it is an error accessing the relative error when the value/magnitude is zero.

## Machine interface
The macros `(quantity ...)` and `#q(...)` are intended as convenience for humans. They are not very lispy. To create quantities in a manner that is suitable for machines, the function `(make-quantity ...)` is defined:
```
(make-quantity :value 1 :error 0.1 :error-type :absolute :unit '((m 1) (s -1)))
```
This is equivalent to `#q(1 +/- 0.1 m / s)`. Note that the unit is a list of unit factors with each unit factor being a symbol that stands for a unit and a power. Being a function, the arguments are evaluated before the `quantity` is created. This allows the unit to be a variable. Units can be created with the `(make-unit ...)` function:
```
(make-unit '(m 1) '(s -1))
```
Units can be converted using the `(convert-unit ...)` function. It accepts either a unit object or a list of unit factors:
```
(convert-unit v (make-unit '(km 1) '(h -1)))
(convert-unit v '((km 1) (h -1)))
```

## Defining new units
New units can be defined using the `(define-unit ...)` macro:
```
(define-unit metre :abbrev m :alias meter)
(define-unit watthour :def (1 watt hour) :abbrev wh)
(define-unit mile :def (1609344/1000 metre) :abbrev mi)
(define-unit gravity :def (981/100 metre / second ^ 2) :abbrev g)
```
For all defined units and abbreviations, more units with all permissible prefixes  such as `KILOWATTHOUR` or `KWH` will automatically be defined.
This may cause naming conflicts which will raise an error.
It is possible to define which prefixes are admissible (see below).

There are a number of keywords that may appear in the definition:

* `:def` Specifies the unit in terms of another unit. Complex dependencies can be given such as (0.3048 metre / second ^ 2). If this keyword is missing, the newly defined unit will be considered a base unit.
* `:alias` Allows multiple (non-abbreviated) names for a unit, given as a single symbol or a list of symbols. This is helpful if there are different spellings (e.g. meter, metre).
* `:abbrev` Abbreviations for the unit can be specified here, given as a single symbol or a list of symbols. Be careful about name conflicts (see above).
* `:prefix-test` A function of two arguments, the base and the power, which decides whether a unit prefix is admissible for use with this unit. For more details, see below.
* `:overwrite` A flag which will suppress errors that occur when a naming conflict is detected.

### Specifying admissible prefixes
For some units, certain prefixes do not make sense. For example: For the unit `TONNE`, the prefix `KILO` is widely used (`KILOTONNE`). It makes little sense, however, to use `MILLITONNE`. To specify, which prefixes are admissible, the keyword parameter `:prefix-test` can be used in the call to `define-unit`. It accepts a function of two arguments, the base and the power of a prefix. The function can then decide, whether such a prefix is admissible by returning `T` or `NIL`. Here is an example:
```
(define-unit tonne :def (1000 kilogram) :prefix-test (lambda (base power) (and (= base 10) (>= power 3))))
```
This will only allow the units `TONNE`, `KILOTONNE`, `MEGATONNE`, etc.

To facilitate the prefix test specification, some functions are provided:

* `(prefix-base base &optional mod)` Will return a function that accepts only the given base and powers divisible by `mod` (defaulting to 1).
* `(prefix-range base from to)` Will return a function that accepts only the given base and powers in the range between `from` and `to` (inclusive). To leave the range open on either side, `NIL` can be supplied.
* `(prefix-list base &rest powers)` Will return a function that accepts only the given base and the given powers.

To combine such tests, the following composing functions are provided:

* `(prefix-and &rest functions)` This will require all functions to accept the prefix.
* `(prefix-or &rest functions)` This will accept a prefix if a single function accepts it.

You can use the standard composing function `complement` where necessary. The above example could be rewritten to read
```
(define-unit tonne :def (1000 kilogram) :prefix-test (prefix-range 10 3 nil))
```

## Defining prefixes
Prefixes can be defined by using the `(define-unit-prefix ...)` macro:
```
(define-unit-prefix giga 9 :abbrev g)
(define-unit-prefix gibi 3 :abbrev gi :base 1024)
```
The full name is specified first, followed by the power. Keyword parameters allow the definition of the base (defaults to 10) and abbreviation. In this example, `giga` is equivalent to 10^9 and `gibi` to 1024^3.

You must define prefixes before defining units.

## Standard units and prefixes
When calling the function `(define-si-units &optional clear-existing-units)`, the SI units will be automatically defined. More sets may be available in the future. Note that the unit prefix abbreviation for `mega` is `meg` because it would otherwise conflict with `milli`.

## License

Physical Quantities: Lisp Library

Copyright (C) 2017 Marco Rossini

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
