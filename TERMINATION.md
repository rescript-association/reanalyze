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

Consider now a recursive version of the program:

```reason
let rec loop = n =>
  if (n.contents > 0) {
    n := n^ - 1;
    print_int(n^);
    loop(n);
  };
```

A pattern begins to merge. There is a recursive function, and some progress happens in its body. And the argument becomes the following: every execution of the loop makes progress. So does the program terminate?
If, in an execution of the program, `loop` is called infinitely often, then progress is made infinitely often. But making progress infinitely often is not possible. Therefore, `loop` is not called infinitely often. Therefore, the program always terminates.

Now let's consider the previous argument for a second. While there's a **global** character to it "the program terminates", there's also a **local** sub-argument: "function `loop` is not called infinitely often".

So a simple strategy for termination amounts to the following: ban while and for loops, and check the recursive functions. Namely, for each recursive function, check that it is not called infinitely often.

Now this is becoming interesting. The termination problem is now broken into a set of sub-problems, one for each function. Also, it's an opening for pragmatism. Wearing a pragmatic hat, one would expect that most non-trivial programs cannot be proven terminating, as they don't always terminate (even those that by design should terminate).

What to do then when presented with a program which, just like most other programs, does not terminate? However, we might know/suspect of a particular way it could fail to terminate. And we are interested in spending energy investigating that one way it might faail to terminate, not all the possible ways.

Breaking up the termination problem allows to do exactly that: if the program is broken into recursive functions, and there is one function (or more) that represents the particular way of non-terminating we are interested in, then we can analyze that function in isolation.

The sub-problem of interest then becomes: given a set of recursive functions, check that none of them is called infinitely often. This will involve several progress measures, e.g. one per recursive function.

We're now getting into the second main difficulty of proving termination: where is the progress measure coming from? It's not realistic to expect the programmer to be versed in creating one, or willing to. There is an entire line of research dedicated to inferring progress measures automatically. It tends to be pretty complex, compute-intensive, and heuristic in style (as otherwise, the halting problem would not be a hard problem). From a user's point of view: unpredictable.

Time to wear a second pragmatic hat. Don't try to infer the progress measure. But ask the user to specify what **they** consider as progress. This is both limiting and liberating. It's limiting as it removes certainty that the program terminates. It's also liberating, as the user can encode anything they wish into the progresss function, thus effectively increasing the number of properties that can be checked. Whether those properties imply termination or not.

One example use of the added freedom is the following., where one can perform hypothetical reasoning. Define a function `fakeProgress` that does nothing but just poses as a progress function. Plus another function `progress` that is actually a progress function. Then the following property can be expressed: of the 15 cases my program goes through, annotate one with the `fakeProgress` function and the remaining 14 with `progress`. This expresses the property: does the program terminate, except for **that** case which I already know does not?
This way for operating is moving towards an opt-in, interactive approach to termination.

But what if I want to have trust in the fact that a program terminates? First, it's possible to approach the problem gradually. One (or more) function at a time.
Wearing a pragmatic hat again: it's not incredibly difficult to stare at a single function long enough, a progress function, and convince yourself that it is a valid measure. Or, you could use a theorem prover. In any case, the pragmatic observation is that the termination function likely changes less often than the program does. So in practice, this setup enables the following process for everyday programming: the program changes over time, and the relationship between the recursive functions changes too, but the progress functions do not change.

This proposed process is not just a hypothetical thought. We have several months of experience in applying it, to at least one nontrivial application. However, there is no claim of generality. Your mileage migth vary.

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

It is not difficult to see, in isolation, that 1) the `progress` function makes progress.
And, it's not difficult to see that 2) the `loop` function cannot be called infinitely often without making progress infinitely often: because `progress` is called each time `loop` is.

The analysis **assumes** 1) and **proves** 2).

# Easy pass

If you don't opt in, nothing is checked, and no issues are reported on the following:

```reason
let rec loop = () => loop();
```

# Easy fail

This time, opt into terminatioon checking with the `@progress` annotation. This time a potential infinite loop is reported.


```reason
[@progress]
let rec loop = () => loop();
```

# Progress function

This time, an actual progress function is defined, which initially does not do anything. This can be used for quick experimentation.
No warnings are reported as every infinite loop makes infinite progress.


```reason
let progress = () => ();

[@progress progress]
let rec loop = () => {
  progress();
  loop();
};
```

# A trivial mistake

See what's wrong now? A possible infinite loop is reporteed.

```reason
let progress = () => ();

[@progress progress]
let rec loop = () => {
  loop();
  progress();
};
```

# Let's get cheeky

It's possivble to play a trick, and use references to implement indirect recursion via the store,
in the style of (Peter) Landin's knot:

```reason
let cheekyRef = ref(() => ());

[@progress]
let rec cheekyLoop = () => {
  cheekyRef := cheekyLoop;
  cheekyRef^();
};
```

This triggers a hygiene violation:

```
cheekyLoop can only be called directly, or passed as labeled argument
```

What's going on is that functions we opt into termination checking, such as `checkyLoop`, are restricted. Storing them in a reference is not allowed.

# No aliasing

Another restriction: you can't alias a function you opted into for termination checking:


```reason
[@progress]
let rec loop = () => loop();
```

That also gives a hygiene violation error:
```
loop can only be called directly, or passed as labeled argument
```

# Mutual recursion

A progress annotation on mutually recursive functions opts them all into termination checking.

This reports an infinite loop

```reason
[@progress]
let rec foo = () => bar()
and bar = () => foo();
```

But this does not report:

```reason
let progress = () => ();

[@progress progress]
let rec foo = () => bar()
and bar = () => {
  progress();
  foo();
};
```

That's becuse every infinite loop through either `foo`, or `bar`, makes progress infinitely often.

# A higher-order riddle

Does this always terminate?

```reason
let next = _ => ();

type token =
  | Int(int)
  | Float(float)
  | Eof;

type t = {token};

[@progress next]
let rec f = p =>
  switch (p.token) {
  | Int(i) => g(p) + i
  | Eof => 0
  | _ =>
    next(p);
    f(p);
  }

and gParam = (p, ~g) => {
  switch (p.token) {
  | Int(i) => g(p) + i
  | _ => f(p)
  };
}

and g = p => {
  next(p);
  gParam(p, ~g);
};
```

According to the analysis, it does.
Notice this shows a higher-order use of functions we opt into for terminaition. Specifically, `gParam` takes `g` as a labeled parameter. So that is the one case which does not trigger a hygiene violation: functions we opt into can either be called directly or passed via labeled arguments.

# Liveness

The properties checked by the analysis can be abstract, and termination is just one of the applications.
Another one is liveness.

Here's an example of a server, which obviously never terminates, yet the analysis checks that it keeps on responding to requests:

```reason
type state;
type request;

let getRequest = (_: state): (request, state) => assert(false);
let processRequest: request => unit = assert(false);

[@progress getRequest]
let rec server = state => {
  let (request, state1) = getRequest(state);
  processRequest(request);
  server(state1);
};
```
