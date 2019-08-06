
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lofi <img src="man/figures/logo.png" align="right" height=230/>

<!-- badges: start -->

![](http://img.shields.io/badge/cool-useless-green.svg)
![](http://img.shields.io/badge/dependencies-zero-blue.svg)
<!-- badges: end -->

The goal of `lofi` is to squeeze multiple, low-fidelity representations
of colours and numbers into the 32-bits of a single, standard integer in
R.

This low-fidelity representation of values (a.k.a. *lofi*) is usually
only an approximation of the original values, and reconstructed values
will most likely be slightly different from the original.

Visit the [webpage](https://coolbutuseless.github.io/package/lofi/) for
more in-depth documentation.

## What’s in the box

  - `pack()` and `unpack()` are the key functions for packing/unpacking
    multiple values into the bits of an integer
  - There is a suite of low-level functions for handling each particular
    supported type e.g. `dbl_to_lofi()` and `lofi_to_dbl()`

## Installation

You can install from [GitHub](https://github.com/coolbutuseless/lofi)
with:

``` r
# install.packages("devtools")
devtools::install_github("coolbutuseless/lofi")
```

# `pack()` and `unpack()`

The `pack()` function will encode a set of values into the bits of a
single integer. The `unpack()` function will reconstruct the original
values from this single integer.

The schematic below illustrates the method by which a list of values is
packed into a single integer and then unpacked back into the list of
values. `pack()` and `unpack()` in turn rely on some lower level
functions `X_to_lofi()` and `lofi_to_X()` (as indicated in the yellow
boxes).

![](man/figures/pack-unpack.png)

The key to the process is the **packing specification** (a.k.a.
`pack_spec`) which defines the type of value you want to store how many
bits are used to store each value.

Steps for using this package:

1.  Define a `pack_spec` defining how each value is converted to lofi
2.  Call `pack()` on a named list of values, along with the `pack_spec`
3.  Receive back a single integer containing all the packed lofi bits
4.  Call `unpack()` on the single integer (along with the `pack_spec`)
5.  Receive a list of reconstructed values which are the same as the
    original values - but maybe with some loss of precision.

The `pack_spec` defines information on packing the following
types:

| type    | lossless? | nbits | description                                   | signed   |
| ------- | --------- | ----- | --------------------------------------------- | -------- |
| integer | Yes       | 1-32  | pack standard integer                         | optional |
| logical | Yes       | 1-32  | standard 1-bit logical. zero-padded if needed | NA       |
| choice  | Yes       | 1-32  | almost like a factor representation           | NA       |
| double  | No        | 1-32  | pack a standard double                        | optional |
| colour  | No        | 3-24  | pack a hex colour e.g. \#123456               | NA       |
| scaled  | No        | 1-32  | pack a range into the given bits              | NA       |
| custom  | Possibly  | 1-32  | user specified functions used to pack/unpack  | NA       |

The integer, logical and choice types are lossless, and original values
can be perfectly reconstructed by `unpack()`. The double, colour and
scaled types all quantize the inputs in some way and lose information -
thus the original value is always *imperfectly* reconstructed (except in
very particular circumstances).

For information on the specification for each type, see `?lofi::pack` or
[`vignette("packing-specification",
package='lofi'`](https://coolbutuseless.github.io/package/lofi/articles/packing-specification.html)

## Example - `pack/unpack` the first row of `iris` data

The `iris` dataset gives the measurements in cm of the variables sepal
length and width, and petal length and width, respectively, for 50
flowers from each of 3 species of iris. The first rows of the data are
shown below:

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
| -----------: | ----------: | -----------: | ----------: | :------ |
|          5.1 |         3.5 |          1.4 |         0.2 | setosa  |
|          4.9 |         3.0 |          1.4 |         0.2 | setosa  |
|          4.7 |         3.2 |          1.3 |         0.2 | setosa  |

First rows of iris data

The `pack_spec` for the data seen in iris is:

  - `Sepal.Length` is a floating point value with 1 decimal place with a
    maximum value of 7.9. This could be multiplied by 10, converted to
    an integer and stored in 7 bits.
  - Similarly for `Sepal.Width`, `Petal.Length` and `Petal.Width` -
    after multiplying by 10, and treating as an integer, these values
    could all by stored in 6, 7, and 5 bits respectively.
  - `Species` is a choice from 3 options, so in the best case we only
    need 2 bits to store this information.

The defined `pack_spec` is stored as a
list:

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Can perfectly pack 'iris' into 27 bits per row.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pack_spec <- list(
  Sepal.Length = list(type = 'integer', nbits = 7, mult = 10, signed = FALSE),
  Sepal.Width  = list(type = 'integer', nbits = 6, mult = 10, signed = FALSE),
  Petal.Length = list(type = 'integer', nbits = 7, mult = 10, signed = FALSE),
  Petal.Width  = list(type = 'integer', nbits = 5, mult = 10, signed = FALSE),
  Species      = list(type = 'choice' , nbits = 2,
                      options = c('setosa', 'versicolor', 'virginica'))
)
```

Now take the first row of `iris` and `pack()` it:

``` r
lofi::pack(iris[1, ], pack_spec)
#> [1] 54052616
```

So the first row of iris has now been packed into the integer:
**54052616**.  
If this integer is viewed as the 32 bits which make it up, the different
lofi data representations can be identified:

![](man/figures/iris-bits.png)

If the integer is now **`unpack()ed`**, we get back the original data.

``` r
lofi::unpack(54052616L, pack_spec)
#> $Sepal.Length
#> [1] 5.1
#> 
#> $Sepal.Width
#> [1] 3.5
#> 
#> $Petal.Length
#> [1] 1.4
#> 
#> $Petal.Width
#> [1] 0.2
#> 
#> $Species
#> [1] "setosa"
```

For an example of using `lofi` to pack an entire data.frame see
[`vignette("packing-a-data-frame",
package='lofi')`](https://coolbutuseless.github.io/package/lofi/articles/packing-a-data-frame.html)

# Conversion to/from low-fidelity representation

Underneath `pack()` an `unpack()` is a suite of low-level functions for
handling each particular supported type

  - `dbl_to_lofi()`, `lofi_to_dbl()`
  - `int32_to_lofi()`, `lofi_to_int32()`
  - `hex_colour_to_lofi()`, `lofi_to_hex_colour()`
  - `lgl_to_lofi()`, `lofi_to_lgl()`
  - `choice_to_lofi()`, `lofi_to_choice()`

## 64-bit (double precision) floating point to Lofi

Double precision floating point values are converted to low-fidelity
representation by truncating the mantissa, and re-encoding the exponent.
Low-fidelity floats have limited range, poorer precision, and will
almost never give back the exact starting value when `unpack()ed`.

Note: `lofi` has no explicit support for `NA`, `NaN`, `Inf` or
[denormalized numbers](https://en.wikipedia.org/wiki/Denormal_number).

The following converts a double into a 10 bit float (with a sign bit,
2-bit exponent and 7-bit mantissa). The reconstructed double is close to
the original value, but not an exact
match.

| Representation                                                    | Bits | Value      | Bit layout                          |
| ----------------------------------------------------------------- | ---- | ---------- | ----------------------------------- |
| Double precision                                                  | 64   | \-1.234    | ![](man/figures/compact-float1.png) |
| Lofi double `dbl_to_lofi(-1.234, float_bits = c(1, 2, 7))`        | 10   | 669L       | ![](man/figures/compact-float2.png) |
| Reconstructed double `lofi_to_dbl(669L, float_bits = c(1, 2, 7))` | 64   | \-1.226562 | ![](man/figures/compact-float3.png) |

For more example of using `lofi` to pack double precision floating point
values see [`vignette("lofi-double",
package='lofi')`](https://coolbutuseless.github.io/package/lofi/articles/lofi-double.html)

## RGB Hex Colour to Lofi

  - Hex colours are converted to lofi representation by considering each
    of the three 8-bit colour channels and quantizing the value into
    fewer bits.
  - The number of bits can be specified individually for the separate R,
    G and B colours.
  - The folowing shows that the reconstructed colour isn’t identical to
    the original, but it is still a reasonable
approximation.

| Representation                                                              | Bits | Value    | Colour sample or bit layout      |
| --------------------------------------------------------------------------- | ---- | -------- | -------------------------------- |
| Original colour                                                             | 24   | \#123456 | ![](man/figures/swatch1.png)     |
| Low-fidelity colour `hex_colour_to_lofi('#123456', rgb_bits = c(3, 3, 2)))` | 8    | 5L       | ![](man/figures/colour-bits.png) |
| Reconstructed colour `lofi_to_hex_colour(5L,  rgb_bits = c(3, 3, 2))`       | 24   | \#002455 | ![](man/figures/swatch2.png)     |

For more example of using `lofi` to pack colours see
[`vignette("lofi-colour",
package='lofi')`](https://coolbutuseless.github.io/package/lofi/articles/lofi-colour.html)

## Integer to Lofi

  - Integers are converted to low-fidelity representation by truncacting
    any leading zeros (or, in the case of negative numbers, truncating
    the leading ones)
  - Keeping a leading bit for the sign is optional, and if it is
    excluded then the lofi representation is only able to hold positive
    numbers
  - `lofi` correctly keeps the sign bit and twos-complement for negative
    values

| Representation                                                       | Bits | Value | bit layout                        |
| -------------------------------------------------------------------- | ---- | ----- | --------------------------------- |
| Original integer                                                     | 32   | \-12  |                                   |
| Low-fidelity integer `int32_to_lofi(-12L, nbits = 5, signed = TRUE)` | 5    | 20L   | ![](man/figures/integer-bits.png) |
| Reconstructed integer `lofi_to_int32(20, nbits = 5, signed = TRUE)`  | 32   | \-12  |                                   |

## Choice to Lofi

  - Convert a value into a zero-based index into a list of options.
  - This is similar in idea to how a `factor` works in R

<!-- end list -->

``` r
options <- c('apple', 'banana', 'carrot', 'dog')
choice  <- c('apple', 'apple', 'dog')

(lofi   <- choice_to_lofi(choice, options))
#> [1] 0 0 3

lofi_to_choice(lofi  , options)
#> [1] "apple" "apple" "dog"
```

# Caveats + Future

  - The `pack/unpack` spec is limited to 32-bits as
    1.  this is my use case.
    2.  that’s the maximum number of bits in an R integer
    3.  `double` values are 64-bits but they don’t have built-in support
        for bitwise logical operations like `bitwAnd()` etc.
  - It should be possible to instead pack/unpack values into a unlimited
    stream of `raw` bytes. This may be considered for future versions.
  - Factors can not be directly packed. You will have to convert to
    another representation (e.g. character) and encode with
    `choice_to_lofi()`.
