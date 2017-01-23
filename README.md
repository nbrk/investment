Library for describing investments or other stateful, time-dependent finance activities. 

The description happens in the Investment monad which is then run for a number of hours, or days; 
maximum granularity for controlling the investment is 1 minute. Investment wraps around the IO monad,
so any external inputs/random can be used within the description right away.
