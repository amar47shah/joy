# Joy

The Joy programming language

This is my attempt at implementing and understanding the Joy programming language. It's hard to find
much in the way of reference material on this language.

```haskell
λ> let s = initialState [JoyNumber 10, JoyNumber 20, JoyNumber 30]
λ> runRecursive (pure s) 0

"Running step 0 State {_input = [JoyNumber 10,JoyNumber 20,JoyNumber 30], _output = [], _env = fromList []}"
"Running step 1 State {_input = [JoyNumber 20,JoyNumber 30], _output = [JoyNumber 10], _env = fromList []}"
"Running step 2 State {_input = [JoyNumber 30], _output = [JoyNumber 20,JoyNumber 10], _env = fromList []}"
Right (State {_input = [], _output = [JoyNumber 30,JoyNumber 20,JoyNumber 10], _env = fromList []})

```

## Combinators

```
                [A] zap  ==
                [A] i    == A
                [A] unit == [[A]]
                [A] rep  == A A
                [A] m    == [A] A
                [A] run  == A [A]
                [A] dup  == [A] [A]
            [B] [A] k    == A
            [B] [A] z    == B
            [B] [A] nip  == [A]
            [B] [A] sap  == A B
            [B] [A] t    == [A] B
            [B] [A] dip  == A [B]
            [B] [A] cat  == [B A]
            [B] [A] swat == [A B]
            [B] [A] swap == [A] [B]
            [B] [A] cons == [[B] A]
            [B] [A] take == [A [B]]
            [B] [A] tack == [B [A]]
            [B] [A] sip  == [B] A [B]
            [B] [A] w    == [B] [B] A
            [B] [A] peek == [B] [A] [B]
            [B] [A] cake == [[B] A] [A [B]]
        [C] [B] [A] poke == [A] [B]
        [C] [B] [A] b    == [[C] B] A
        [C] [B] [A] c    == [B] [C] A
        [C] [B] [A] dig  == [B] [A] [C]
        [C] [B] [A] bury == [A] [C] [B]
        [C] [B] [A] flip == [A] [B] [C]
        [C] [B] [A] s    == [[C] B] [C] A
    [D] [C] [B] [A] s'   == [[D] C] A [D] B
    [D] [C] [B] [A] j    == [[C] [D] A] [B] A
[E] [D] [C] [B] [A] j'   == [[D] A [E] B] [C] B
```

## References

+ http://tunes.org/~iepos/joy.html
+ http://www.complang.tuwien.ac.at/anton/euroforth/ef01/thun01.pdf
+ https://github.com/nickelsworth/sympas/blob/master/text/18-minijoy.org


