# Day 3: Joltage Calculation

## Algorithm

### Python Implementation
```py
import sys

BATTERIES = 12

with open(sys.argv[1], "r") as f:
    acc = 0
    for line in f.read().split():
        inc_pairs = []
        # Modeled where idx 0 is most significant and idx BATTERIES - 1 is least significant
        bank = [0] * BATTERIES
        prev = -1
        joltage = 0
        for idx, char in enumerate(line):
            val = ord(char) - ord("0")
            if idx < BATTERIES:
                bank[idx] = val
                if val > prev and idx != 0:
                    inc_pairs += [idx]
            else:
                # If there are increasing pairs
                if len(inc_pairs) > 0:
                    bank = bank[: inc_pairs[0] - 1] + bank[inc_pairs[0] :] + [val]
                    inc_pairs = [i - 1 for i in inc_pairs]
                    if (
                        inc_pairs[0] >= 1
                        and bank[inc_pairs[0]] > bank[inc_pairs[0] - 1]
                    ):
                        pass
                    else:
                        inc_pairs = inc_pairs[1:]
                    if bank[-1] > bank[-2]:
                        inc_pairs += [BATTERIES - 1]
                elif val > bank[-1]:
                    bank = bank[:-1] + [val]
                    if bank[-1] > bank[-2]:
                        inc_pairs += [BATTERIES - 1]
            prev = val
        for value in bank:
            joltage = (joltage * 10) + value
        acc += joltage
    print(acc)
```

### Description
In the above implementation we have two seperate memory regions that are used to store intermediate information. First we `bank` which is ued to store the values of the batterise with the first battery stored at the 0 position and the last battery stores at the 11 position. We also have `inc_pairs` which is a FIFO queue storing the index of batteries that have a larger value than the battery in the position one more significant. For example if the bank was equal to `[9, 8, 7, 6, 7, 6, 5, 4, 3, 2, 1, 2]` the FIFO queue would be equal to `[4, 11]`.

The algorithm starts by filling the bank with the first 12 elements and adding the indexes of increasing elements to the queue. For each of the next elements we have two different options on how to process it
1. We have increasing elements and we need to shift elements
2. We dont have any increasing elements and we just need to replace the last element if the new element is greater than the current element.

In the case that we are in case 1 there are 4 steps we need to take.
1. We need to shift all the elements from the index at the front of the queue to the end up one position and insert the current element at the end.
2. We need to decrement all the elements in the queue by one.
3. We need to determine if we shifted an element that is smaller than the one it is now next to and if so pop the element off the queue
4. If the new element is greater than the previous last element add 11 to the queue

If we are in case 2 and the new element is greater than the last element then there are 2 steps to take
1. Replace the last element with the new element
2. If the new element is greater than the second to last element add 11 to the queue

If we are in case 2 and the new element is not greater than the last element then we do nothing.

Once we have processed all the elements we need to convert the bank from the binary coded decimal-esque array format to a decimal number so we can accumulate it.

## Hardware Design

### Bank Memory

The bank memory needs the ability to do two seperate operations. First it needs to have the ability to write into the last position of the bank, and second it need to have the ability to shift specific elements one position up.

We also need to be able to read from two positions at once asynchronously, because the second element we want to read is also at the index one less than the first index we only need to take in the larger index.

### FIFO Queue

Our FIFO Queue needs to implement a standard circular queue but it also needs to be able to simultaneously decrement all the items in the queue.

### BCD to Binary

We need a design that can stream in 4 bit digits that range from 0 to 9 and accumulate them together into a binary number. This is easily realised by for each digit multiplying the current value by 10 and adding the new digit.

### Calculation State Machine

Our main calculation has five different states.
1. Waiting for the start of a new calculation
2. Filling the bank with the first 12 elements
3. Updating the bank for a new element
4. Updating the queue for a new element
5. Converting the bank to a binary number

In state `3` we handle steps `1, 2, and 4` in the case we have an element in the queue and steps `1 and 2` in the cse that we dont.

In state `4` if we had an element in the queue we need to do step `3` here and if we didnt we are done. Next we move back to state `3` if there are more elements and onto state `5` if we are done.