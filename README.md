# Joy

The Joy programming language

## Combinators

http://tunes.org/~iepos/joy.html

This is my attempt at implementing and understanding the Joy programming language. It's hard to find
much in the way of reference material on this language.

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
