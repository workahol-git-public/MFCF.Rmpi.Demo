#
# A demonstration of Monte Carlo integration on Rmpi.
#

library(Rmpi)

# Start MPI.
print("Starting MPI.")
n_slaves <- 16
mpi.spawn.Rslaves(nslaves=n_slaves)


# Function to be integrated.
integrand <- function(x) { return(exp(x)); }
x_min <- 0.0;
x_max <- 1.0;

# MC parameters.
n <- 10 ** 5
n_per_node <- n / n_slaves 	# Master does not do work here.

runLongWay <- function() 
		
	#
	# Writing the MC the long way...
	#

	print("Running the 'long way'...")

	# Send slaves information.
	print("Sending slaves information....")
	mpi.bcast.Robj2slave(integrand)
	mpi.bcast.Robj2slave(n_per_node)
	mpi.bcast.Robj2slave(x_min)
	mpi.bcast.Robj2slave(x_max)

	# Tell slaves to run Monte Carlo
	print("Telling slaves to run simulation.")
	mpi.remote.exec(
		slave_sum <- sum(sapply(runif(n_per_node, x_min, x_max), integrand))  
	)

	# Slaves pass back to master.
	print("Gathering results using reduce.")
	slave_sum <- 0.0
	mpi.remote.exec(mpi.reduce(slave_sum, 2, "sum", 0, 1))
	mc_estimate <- mpi.reduce(slave_sum, 2, "sum")

	# Compute final MC estimate
	print("Final MC estimate:")
	print(mc_estimate / n)

fApply <- function(n_per_node, x_min, x_max)
	
	# This is what each core does in the apply.`
	return(sum(sapply(runif(n_per_node, x_min, x_max), integrand))) 

useMpiApply <- function()

	#
	# Using mpi.apply...
	#

	print("Using mpi.apply...")

	# Note that mpi.apply returns a list...
	mc_estimate <- sum(
		unlist(mpi.apply(rep(n_per_node,n_slaves), fApply, x_min=x_min, x_max=x_max))
	) / n 

	print("Final MC estimate:")
	print(mc_estimate)

# run both ways.
useMpiApply()
runLongWay()

mpi.finalize()
