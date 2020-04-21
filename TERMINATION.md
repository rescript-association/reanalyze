## Pragmatic Termination

If you never know when it's time to stop, don't worry. Computers don't either. The so called Halting Problem refers to the fact that a program cannot determine whether another arbitrary program terminates or not.

```reason
let loop = n => {
  while (n.contents > 0) {
    n := n.contents - 1;
    print_int(n^);
  };
};
```

Does this program terminate for all integers `n`? How do I make sure it does?

The typical argument goes as follows. Consider a progress function on the state, and check that it's not possible to make progress infinitely often. In the example, the progress function is the distance from `n.contents ` to zero. At each iteration the number descreases, and it cannot decrease infinitely often. Therefore, the program terminates.

Consider now the recursive version of the program:

```reason
let rec loop = n =>
  if (n.contents > 0) {
    n := n^ - 1;
    print_int(n^);
    loop(n);
  };
```

Then a pattern begins to merge. There is a recursive function, and some progress happens in its body. And the argument becomes the following: every execution of the loop makes progress. So does the program terminate?
If in an execution of the program `loop` is called infinitely often, then progress is made infinitely often. But that is not possible. Therefore `loop` is not called infinitely often. Therefore the program terminates.

Now let's consider the previous argument for a second. While there's a **global** character to it "the program terminates", there's also a **local** sub-argument: "function `loop` is not called infinitely often".

So a simple strategy for termination amounts to the following: ban while and for loops, and check the recursive functions. Namely, for each recursive function, check that it is not calleed infinitely often.

Now this is becoming interesting. The termination problem is now broken into a set of sub-problems. Also, it's an opening for pragmatism. Wearing a pragmatic hat, I would expect that most non-trivial program cannot be proven terminating, as they don't always terminate (even those that by design should terminate).

What to do then when presented with a program which, just like most other programs, does not terminate. But, we know of a particular way it could fail to terminate. And we are interested in spending energies investigating that one way, not all the possible ways.

Breaking up the termination problem allows to do exactly that: if the program is broken into recursive functions, and there is one function (or more) that represents the particular way of non-terminating I am interested in, then I can analyze that function in isolation.

The sub-problem of interest then becomes: given a set of recursive functions, check that none of them is called infinitely often. This will involve several progress measures, e.g. one per recursive function.

We're now getting into the second main difficulty of proving termination: where is the progress measure coming from. It's not realistic to expect the programmer to be versed in creating one, or willing to. There is a line of work dedicated to inferring progress measures.
It tends to be pretty complex, and compute intensive, and heuristic in style (as otherwise, the halting problem would not be a hard problem): unpredictable.

Time to wear a second pragmatic hat. Don't try to infer the progress measure. But ask the user to specify what **they** consider as progress. This is both limiting and liberating. It's limiting as it removes certainty that the program terminates. It's also liberating, as the user can encode anything they wish into the progresss function, thus effectively increasing the number of properties that can be checked. Whether those properties imply termination or not.

One example use of the added freedom is the following. One can perform hypothetical reasoning. So define a function `fakeProgress` that does nothing else but pretending to make progress. Plus another function `progress` that is actually a progress function. Then the following property can be expressed: of the 15 cases of this function, annotate one with the `fakeProgress` function and the remaining with `progress`. This expresses the property: does the function terminate, except for **that** case which I already know does not?
This setup is moving towards a more opt-in, interactive approach to termination.

But what if I want to have trust in a program terminating. First, it's possible to approach the problem gradually. One (or more) function at a time.
Wearing a pragmatic hat again: it's not incredibly difficult to stare at a single function, a progress function, and convince yourself that it is a valid measure. Or, you could use a theorem prover. In any case, the pragmatic observation is that the termination function likely changes less often than the program does. So in practice, this setup enables the following process for everyday programming: the program changes over time, and the relationship between the recursive functions changes too, but the progress functions do not change.

This process is not just a nice thought. We have multi-months of experience in applying it, to at least one nontrivial application. However, there is no claim of generality. Your mileage migth vary.

Now back to the example:

```reason
let progress = n =>
  if (n.contents > 0) {
    n := n.contents - 1;
    true;
  } else {
    false;
  };

let rec loop = n =>
  if (progress(n)) {
    print_int(n^);
    loop(n);
  };
```

It's not difficult to see, in isolation, that the `progress` function makes progress.
And, it's not difficult to see that the `loop` function cannot be called infinitely often without making progress infinitely often: because `progress` is called each time `loop` is.

## TODO

- The `@progress` annotation.
- The real examples.
- Hygiene restrictions (so relevant functions cannot escape).
- Higher-order cases.

Cheecky example:

```reason
let cheecyRef = ref(() => ());

let rec cheecyLoop = () => {
  cheekyRef := cheeskyLoop;
  cheekyRef^();
};
```









