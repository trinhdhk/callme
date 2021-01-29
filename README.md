# Making objects callable

A proof-of-concept package which convert <code>R</code> lists and environments to a (pseudo-)callable objects, just like how <code>Python</code> behaves.
It also includes a special extensions of R6 Class, which produces callable R6 generators and (optionally) callable R6 objects.

<p style="background:#e6de70">This is an experimental package using brute-force approaches; things can change at any time. More efficient approaches are welcomed.</p>

<center>![](inst/MakeCallable.png)</center>

## Installation

```r
# install.packages("remotes")
remotes::install_github("trinhdhk/callme", ref="main")
```

## Usage

Objects can be created via <code>make_callable</code>.

``` r
my_list <- list(a = 3, b = 4, .call = function(x) cat("Hello", x))
my_list <- callme::make_callable(my_list)
#> This is an experimental package. Use at your own risk.
my_list
#> Callable object at <environment: 0x000000000804b120>
#> $b
#> [1] 4
#> 
#> $a
#> [1] 3
my_list("folks")
#> Hello folks
```

An callable R6 (generator) object can be created via <code>R6CallClass</code>.
Call target must be in the public field.

``` r
MyCallClass <- callme::R6CallClass(classname = "MyCallClass",
                           public = list(
                             initialize = function(){
                               cat("Hello")
                               self
                             },
                             print = function() {
                               cat("I am R6Call")
                               invisible(self)
                             },
                             .call = function(x){
                               cat("Hello", x)
                               invisible(self)
                             }
                           ), callable_object_target = c(".call"))
#> This is an experimental package. Use at your own risk.

mycallobj <- MyCallClass()
#> Hello
mycallobj
#> I am R6Call
mycallobj("folks")
#> Hello folks
```

Prost!
