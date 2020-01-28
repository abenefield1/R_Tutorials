# Amy Benefield
# ODEs in R Tutorial

library(deSolve)


############################################################################
############################################################################
# 1. A simple ODE: chaos in the atmosphere
############################################################################
############################################################################

############################################################################
# Lorenz Equations
############################################################################
# Lorenz Equations are 3 DEs (X, Y, Z) representing earth's atmosphere. It's an initial value problem (IVP).

############################################################################
# Model Specification: define parameters, state variables and their initial values, and writing model equations

# Setting model parameters, a, b, c:
parameters<-c(a=-8/3,
              b=-10,
              c=28)

# Setting state variables, X, Y, Z:
state<-c(X=1,
         Y=1,
         Z=1)

# Model equations. Equations are specified in a function that we'll name, Lorenz. Input to function (in order) = model time (t),values of state variables (state), and parameters. Parameter and state variables converted from vectors to a list: with(as.list(c(state, parameters))
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

############################################################################
# 1.2 Model Application:

# Time specification: Run the model for 100 days, and give output at 0.01 daily intervals.
times<-seq(0, 100, by = 0.01)

# Model integration: solve using ode function from deSolve (uses default integration). Function ode takes as input, a.o. the state variable vector (y), the times at which output is required (times), the model function that returns the rate of change (func) and the parameter vector (parms). Function ode returns an object of class deSolve with a matrix that contains the values of the state variables (columns) at the requested output times.
out<-ode(y = state, times = times, func = Lorenz, parms = parameters)
head(out)

# Plotting Results: Use plot method for class of deSolve objects. Outer upper margin increased to allow for figure heading (mtext).
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-") # Plotted vs. time
plot(out[, "X"], out[, "Z"], pch = ".") # Z vs. X
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)


############################################################################
############################################################################
# 2. Solvers for initial value problems of ordinary differential equations
############################################################################
############################################################################
# **deSolve** has several IVP ODE solvers. All integration methods can be used by settting ode's argument method, or can be run as stand alone functions. for each integration routine, several options are available to optimise performance. The next statements will use integration method **radau** to solve the model, and set the tolerances to a higher value than the default. Both statements are the same:
outb <- radau(state, times, Lorenz, parameters, atol = 1e-4, rtol = 1e-4)
outc <- ode(state, times, Lorenz, parameters, method = "radau",
            atol = 1e-4, rtol = 1e-4)
# The default integration method switches automatically between stiff and non-stiff systems. This is a very robust method, but not necessarily the most efficient solver. For most cases, the default solver, **ode** and using the default settings will do. To show how to trigger the various methods, we solve the model with several integration routines, each time printing the time it took (in seconds) to find the solution:
print(system.time(out1 <- rk4 (state, times, Lorenz, parameters)))
print(system.time(out2 <- lsode (state, times, Lorenz, parameters)))
print(system.time(out <- lsoda (state, times, Lorenz, parameters)))
print(system.time(out <- lsodes(state, times, Lorenz, parameters)))
print(system.time(out <- daspk (state, times, Lorenz, parameters)))
print(system.time(out <- vode (state, times, Lorenz, parameters)))

############################################################################
# 2.1 Runge-Kutta methods and Euler
