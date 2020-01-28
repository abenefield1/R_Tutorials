ODE Tutorial
================
Amy Benefield
1/28/2020

Libraries:

``` r
library("deSolve")
```

# 1\. A simple ODE: chaos in the atmosphere

We’re using the Lorenz equations as the first example. Following along
from tutorial
(<https://cran.r-project.org/web/packages/deSolve/vignettes/deSolve.pdf>).
The Lorenz equations are 3 DEs (X, Y, Z) representing earth’s
atmosphere. It’s an initial value problem (IVP). First, we must specify
the model, and then we implement it.

## 1.1 Model Specification:

Model Specification: define parameters, state variables and their
initial values, and writing model equation. \#\#\# Setting model
parameters, a, b, c:

``` r
parameters<-c(a=-8/3,
              b=-10,
              c=28)
```

### Setting state variables, X, Y, Z:

``` r
state<-c(X=1,
         Y=1,
         Z=1)
```

### Defining Model Equations:

Equations are specified in a function that we’ll name, Lorenz. Input to
function (in order) = model time (t),values of state variables (state),
and parameters. Parameter and state variables converted from vectors to
a list: with(as.list(c(state, parameters))

``` r
Lorenz<-function( t, state, parameters){
  with(as.list(c(state, parameters)),{
    # rate of change:
    dX <- a*X + Y*Z
    dY <- b * (Y-Z)
    dZ <- -X*Y + c*Y - Z
    
    # return the rate of change:
    list(c(dX, dY, dZ))
  }) # closing/ending the with(as.list(c(state, parameters)) code
}
```

## 1.2 Model Application:

## Time specification:

Run the model for 100 days, and give output at 0.01 daily intervals.

``` r
times<-seq(0, 100, by = 0.01)
```

### Model Integration:

Solve the model using the **ode** function from **deSolve** (uses
default integration). Function ode takes as input, a.o. the state
variable vector (y), the times at which output is required (times), the
model function that returns the rate of change (func) and the parameter
vector (parms). Function ode returns an object of class deSolve with a
matrix that contains the values of the state variables (columns) at the
requested output times.

``` r
out<-ode(y = state, times = times, func = Lorenz, parms = parameters)
head(out)
```

    ##      time         X        Y        Z
    ## [1,] 0.00 1.0000000 1.000000 1.000000
    ## [2,] 0.01 0.9848912 1.012567 1.259918
    ## [3,] 0.02 0.9731148 1.048823 1.523999
    ## [4,] 0.03 0.9651593 1.107207 1.798314
    ## [5,] 0.04 0.9617377 1.186866 2.088545
    ## [6,] 0.05 0.9638068 1.287555 2.400161

### Plotting Results:

Use plot method for class of deSolve objects. Outer upper margin
increased to allow for figure heading (mtext).

``` r
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-") # Plotted vs. time
plot(out[, "X"], out[, "Z"], pch = ".") # Z vs. X
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)
```

![](ODE_Tutorial_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# 2\. Solvers for initial value problems of ordinary differential equations

**deSolve** has several IVP ODE solvers. All integration methods can be
used by settting ode’s argument method, or can be run as stand alone
functions. for each integration routine, several options are available
to optimise performance. The next statements will use integration method
**radau** to solve the model, and set the tolerances to a higher value
than the default. Both statements are the
same:

``` r
outb <- radau(state, times, Lorenz, parameters, atol = 1e-4, rtol = 1e-4)
outc <- ode(state, times, Lorenz, parameters, method = "radau",
            atol = 1e-4, rtol = 1e-4)
```

The default integration method switches automatically between stiff and
non-stiff systems. This is a very robust method, but not necessarily the
most efficient solver. For most cases, the default solver, **ode** and
using the default settings will do. To show how to trigger the various
methods, we solve the model with several integration routines, each time
printing the time it took (in seconds) to find the solution:

``` r
print(system.time(out1 <- rk4 (state, times, Lorenz, parameters)))
```

    ##    user  system elapsed 
    ##   0.418   0.003   0.424

``` r
print(system.time(out2 <- lsode (state, times, Lorenz, parameters)))
```

    ##    user  system elapsed 
    ##   0.168   0.004   0.173

``` r
print(system.time(out <- lsoda (state, times, Lorenz, parameters)))
```

    ##    user  system elapsed 
    ##   0.212   0.000   0.215

``` r
print(system.time(out <- lsodes(state, times, Lorenz, parameters)))
```

    ##    user  system elapsed 
    ##   0.153   0.000   0.154

``` r
print(system.time(out <- daspk (state, times, Lorenz, parameters)))
```

    ##    user  system elapsed 
    ##   0.215   0.000   0.215

``` r
print(system.time(out <- vode (state, times, Lorenz, parameters)))
```

    ##    user  system elapsed 
    ##   0.152   0.000   0.153

## Runge-Kutta methods and Euler: