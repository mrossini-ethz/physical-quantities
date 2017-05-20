# Physical Quantities

This lisp library handles physical quantities which consist of

 * Value/Magnitude
 * Uncertainty/Error
 * Unit

where the type of the value can be any subtype of `real`. For the uncertainty, both absolute and relative values are possible. Combinations of lisp symbols or strings are used to describe units. User defined units including abbreviations and prefixes are supported. Error propagation and unit checking is performed for all defined operations.

## Example usage
The following example illustrates the use of the library:
```
(require :asdf)
(asdf:load-system :physical-quantities)
(use-package :physical-quantities}
(define-si-units)
(define-read-macros)

(let (m c m)
  ;; Define the mass
  (setf m #q(1.00 +/- 0.01 kg))
  ;; The speed of light
  (setf c #q(299792458 m / s))
  ;; e = m * c^2
  (setf e (q* m (qpow c 2)))
  ;; Print e, converted to petajoule
  (print #q(e -> PJ)))
```
This will print
```
#<QUANTITY 89.87552 +/- 1.0 % petajoule {...}>
```
## Defining quantities
To define a quantity, either the macro `(quantity ...)` or the read macro `#q(...)` can be used. For the latter, the function `(define-read-macros)` must be called first.

The following lines of code are equivalent:
```
(quantity 1 "kilogram")
(quantity 1 "kg")

(define-read-macros)
#q(1 kilogram)
#q(1 kg)
```
This creates an object of type `quantity`. Its value is the integer `1` and its unit is `kilogram`. The uncertainty/error of this quantity is zero because it was not specified. To define it, the symbol `+/-` (or `+-`), followed by a number, can be inserted into the macro call after the value:
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
kilogram metre ^ 2 / second ^ 2 / kelvin / mole
kg m ^ 2 / s ^ 2 / K / mol
```
Please note that separating symbols with spaces is compulsory.

### Unit abbreviations
Units can be abbreviated. This means that `kilometre` is interpreted in the same way as `km`. Note that both the unit `metre` and the prefix `kilo` is abbreviated. Mixing abbreviation (e.g. `kmetre` or `kilom`) is not supported.

### Standalone units
Units without value can be obtained by using one of these methods:
```
(mkunit "metre" / "second")
(mkunit "m" / "s")
#u(metre / second)
#u(m / s)
```
These will all create the same unit. Note that the representation of the result may change in the future.

### Upper- and lowercase
Lisp by default converts all symbols that it reads to uppercase. This default setting is disabled and case is preserved for units within the `#q(...)` and `#u(...)` read macros, therefore `#q(1 Pa)` has different units from `#q(1 pA)`. When using the `(quantity ...)` or `(mkunit ...)` macros, this is not possible and therefore you would have to specify `(quantity 1 "Pa")` or `(quantity 1 "pA")`. The macros `(quantity ...)` and `(mkunit ...)` *do* accept symbols, but these will be converted to uppercase by the reader (usually causing a unit lookup error) unless they are escaped by using the `|...|` syntax for example.

Note that the `#q(...)` read macro makes the usual case conversion for the value/magnitude and uncertainty/error. Therefore, it is possible to write
```
(let ((val 1.0) (err 0.1))
  #q(val +/- err m / s))
```
which will result in `#<QUANTITY 1.0 +/- 0.1 metre / second {...}>`. The symbols are converted to uppercase in the `let` form as well as in the `#q(...)` read macro.

## Operations
Common Lisp does not easily allow the redefinition/overloading of standard operators such as `+` or `*`. For this reason, a number of operators are provided to work with both types `real` and `quantity`. These are prefixed with the letter `q`, e.g. `q+` or `q*`. Example:
```
(q* a (q+ b (qsqrt c)))
```
The result of such an operation is always a quantity, even if all arguments passed to the function are of type `real`.

When using these operations, error propagation will be performed automatically.  The error propagation is first-order only and correlations are not taken into account.

### Defining new operations
Apart from the predefined operations, new options can be defined as normal functions or methods.
See section [Accessing value, error and unit](#accessing-value-error-and-unit) for relevant information.

There is a convenience macro `defqop` that automatically converts arguments to values of type `quantity` if a `real` number is passed:
```
(defqop factorial (number!u)
  (unless (errorlessp number)
    (error "The error propagation of FACTORIAL is undefined."))
  ;; Insert more tests here ...
  (make-quantity :value (loop for n from 1 upto (value number) for result = 1 then (* result n) finally (return result))))
```
The list of arguments (here only `number!u`) is not a lambda list and things like `&optional` or `&key` are not allowed.
Furthermore, all arguments are expected to be either of type `real` or `quantity`.
The ending `!u` in `number!u` means that the argument `number` should automatically be checked for unitlessness.
Otherwise an error will be signaled.
The suffix must only appear in the argument list.

## Converting units
Units can be converted by calling the `(quantity ...)` or `#q(...)` macro and specifying `->` and a new unit:
```
(let ((v #q(20 m / s)))
  (setf v #q(v -> km / h))
  (print v))
```
This would print
```
#<QUANTITY 72 kilometre / hour {...}>
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
(make-quantity :value 1 :error 0.1 :error-type :absolute :unit '(("m" 1) ("s" -1)))
```
This is equivalent to `#q(1 +/- 0.1 m / s)`. Note that the unit is a list of unit factors with each unit factor being a symbol or string that stands for a unit and an integer for the power. Being a function, the arguments are evaluated before the `quantity` is created. This allows the unit to be a variable. Units can be created with the `(make-unit ...)` function:
```
(make-unit '("m" 1) '("s" -1))
```
Units can be converted using the `(convert-unit ...)` function. It accepts either a unit object or a list of unit factors:
```
(convert-unit v (make-unit '("km" 1) '("h" -1)))
(convert-unit v '(("km" 1) ("h" -1)))
```

## Defining new units
New units can be defined using the `(define-unit ...)` macro:
```
(define-unit "metre" :abbrev "m" :alias "meter")
(define-unit "watthour" :def (1 "watt" "hour") :abbrev "Wh")
(define-unit "mile" :def (1609344/1000 "metre") :abbrev "mi")
(define-unit "gravity" :def (981/100 "metre" / "second" ^ 2) :abbrev "g")
```
There are a number of keywords that may appear in the definition:

* `:def` Specifies the unit in terms of another unit. Complex dependencies can be given such as `(0.3048 "metre" / "second" ^ 2)`. If this keyword is missing, the newly defined unit will be considered a base unit.
* `:alias` Allows multiple (non-abbreviated) names for a unit, given as a single symbol or a list of symbols. This is helpful if there are different spellings (e.g. metre, meter).
* `:abbrev` Abbreviations for the unit can be specified here, given as a single symbol or a list of symbols. Be careful about name conflicts (see above).
* `:prefix-test` A function of two arguments, the base and the power, which decides whether a unit prefix is admissible for use with this unit. For more details, see below.
* `:overwrite` A flag to suppress errors that occur when a naming conflict is detected.

For all defined units and abbreviations, more units with all permissible prefixes  such as `kilowatthour` or `kWh` will automatically be defined.
This may cause naming conflicts which will raise an error.
It is possible to define which prefixes are admissible.

### Specifying admissible prefixes
For some units, certain prefixes make no sense. For example: For the unit `tonne`, the prefix `kilo` is widely used (`kilotonne`). It makes little sense, however, to use `millitonne`. To specify, which prefixes are admissible, the keyword parameter `:prefix-test` can be used in the call to `define-unit`. It accepts a function of two arguments, the base and the power of a prefix. The function can then decide, whether such a prefix is admissible by returning `T` or `NIL`. Here is an example:
```
(define-unit "tonne" :def (1000 "kilogram") :prefix-test (lambda (base power) (and (= base 10) (>= power 3))))
```
This will only allow the units `tonne`, `kilotonne`, `megatonne`, etc.

To facilitate the prefix test specification, some functions are provided:

* `(prefix-base base &optional mod)` Will return a function that accepts only the given base and powers divisible by `mod` (defaulting to 1).
* `(prefix-range base from to)` Will return a function that accepts only the given base and powers in the range between `from` and `to` (inclusive). To leave the range open on either side, `NIL` can be supplied.
* `(prefix-list base &rest powers)` Will return a function that accepts only the given base and the given powers.

To combine such tests, the following composing functions are provided:

* `(prefix-and &rest functions)` This will acccept a prefix if all functions accept it.
* `(prefix-or &rest functions)` This will accept a prefix if a single function accepts it.

You can use the standard composing function `complement` where necessary.

The above example could be rewritten to read
```
(define-unit "tonne" :def (1000 "kilogram") :prefix-test (prefix-range 10 3 nil))
```

## Defining prefixes
Prefixes can be defined by using the `(define-unit-prefix ...)` macro:
```
(define-unit-prefix "giga" 9 :abbrev "G")
(define-unit-prefix "gibi" 3 :abbrev "Gi" :base 1024)
```
The full name is specified first, followed by the power. Keyword parameters allow the definition of the base (defaults to 10) and abbreviation. In this example, `giga` is equivalent to 10^9 and `gibi` to 1024^3.

You must define prefixes before defining the units that use them.

## Standard units and prefixes
When calling the function `(define-si-units &optional clear-existing-units)`, the SI units will be automatically defined. More sets may be available in the future.

## Local namespaces
Several unit and prefix definitions can be used in a program by locally defining them. This can be done with the `(with-local-units ...)` and `(with-saved-units ...`) macros. The former completely clears the outside units and prefixes until control leaves the form. The latter makes a copy of all the unit definitions such that they can be changed within the body of the form without affecting the outisde definitions.

Please note that the unit definitions use dynamic scope an not lexical scope. This means that the unit definitions are only local during the time spent within the macro call. It is therefore not possible to close over the unit definitions. While quantities with local unit can be returned from this macro call, the unit may be undefined after the time of the call or may have a different meaning.

## Errors/Conditions
The errors/conditions signaled by the library are all subtypes of `physical-quantities-error` which itself is a subtype of `standard-error`.
However, no event triggers the `physical-quantities-error` directly.
Instead, the most specific error is signaled.
Here is the complete hierarcy of conditions:

* `quantity-definition-error`
  * `quantity-definition-syntax-error`: There is a syntax error in one of the following forms: `#q(...)`, `(quantity ...)`, `(make-quantity ...)`, `#u(...)`, `(make-unit ...)` or `(mkunit ...)`.
  * `quantity-definition-semantic-error`: There is a semantic error in one of the following forms: `#q(...)`, `(quantity ...)`, `(make-quantity ...)`, `#u(...)`, `(make-unit ...)` or `(mkunit ...)`.

* `invalid-unit-error`
  * `invalid-unit-operation-error`: The mathematical operation can not be performed on the input quantity with the given unit.
  * `invalid-unit-conversion-error`: The units can not be converted into each other.
  * `invalid-unit-reference-error`: The unit is undefined or the  prefix unit combination does not exist.

* `operation-undefined-error`: The mathematical operation is undefined for the given input parameters.

* `error-propagation-error`: The propagation of uncertainty can not be performed with the given parameters.

* `unit-definition-error`
  * `unit-definition-syntax-error`: A syntactic problem exists with a `(define-unit ...)` or `(define-prefix ...)` form.
  * `unit-definition-semantic-error`: There is a semantic error with a `(define-unit ...)` or `(define-prefix ...)` form.
  * `unit-definition-conflict-error`: The unit or prefix is already defined or a combination of prefix and unit conflicts with another unit.

## License

Physical Quantities: Lisp Library

Copyright (C) 2017 Marco Rossini

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
