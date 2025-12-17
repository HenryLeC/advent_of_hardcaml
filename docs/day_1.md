# Secret Entrance Part 2

This challenge is very easy to implement in the most basic implementation of purely incrementing or decrementing the current position modulo 100 for each line of input and tracking the number of times you hit 0. This is extremely slow and would be at worst O(n) in the total number of clicks of the dial. We want a solution that is closer to O(k) where k is the number of lines of input.

My first idea when coming up with a smarter solution was that the minimum number of times passing 0 in a single step is the floored division of the positive offset and 100. The maxmimum number is one greater than this. For example turning 50 places in either direction may pass 0 1 time or 0 times. On the other hand if we turn 110 places we are required to have passed 0 at least once but we could have also passed 2 times.

Now we have simplified the solution down to determing whether we passed 0 exactly `abs(offset) // 100` times or one more time than that.

We can split this problem into two problems the case where we are turning right ie (offset > 0) and the case where we are turning left.

In the case we are turning right we can see that if we are at any location on the dial and `position + (offset % 100) >= 100` we passed 0 one extra time. We can also be sure this works in the edge cases of position being 0 and offset being 100.

In the case we are turning left it gets a little more complicated. If we are moving left by 2 positions `offset = -2` and `-2 % 100 = 98` (floored division definition of modulus) and `position + (offset % 100)` turns out to be less than or equal to 100 if we pass 0 an extra time, except in the case that `position == 0` where it will always be less than 100. We can rememdy this by setting `position = 100` in the case that `position == 0` before checking this.

Looking through all the edge cases we can see that we are msising proper handling of the case that `offset = -100` and `position = 0`. We can add an extra check for this specific case. Or we can change from setting the maximum rotations from `abs(offset) // 100` to `offset // 100` and `-(offset // 100) - 1` in the negative case. This also solves the edge case and is what we will be using because we will see it is slightly easier in this case.

I started by implementing the division and modulus operation, in this case because 100 is not a power of two we cannot implement the modulus or division with bitwise operators so I chose a simple loop and subtraction.

Through our algorithm we determine we only need to compute two seperate modulus and one floored division, we need to compute `offset % 100`, `offset // 100` and `position + offset % 100`. By creating a seperate module for division and modulus we can instantiate this module twice once for each of the two inputs.

Because we need to stream inputs and our div_mod operand takes an uncertain number of clock cycles I decided to structure the hardware as a state machine with 5 states,
- Wait_for_start
- Process_stream
- Wait_for_div
- Process_results
- Output

`Wait_for_start` simple waits until the input stream sets a start signal high
`Process_stream` latches the input and starts the division and modulus operations
`Wait_for_div` stalls until the computations are complete
`Process_results` changes the zero_count and position based on the above rules
`Output` stops the computation and outputs the results.