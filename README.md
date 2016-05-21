# Joy

The Joy programming language

## Combinators

http://tunes.org/~iepos/joy.html

http://www.complang.tuwien.ac.at/anton/euroforth/ef01/thun01.pdf
https://github.com/nickelsworth/sympas/blob/master/text/18-minijoy.org

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


```ps
PROCEDURE joy(node : memrange);
VAR nod,temp1,temp2 : memrange;

    PROCEDURE binary(o : standardident; v : integer);
    BEGIN s := kons(o,v,m[m[s].nxt].nxt) END;

    FUNCTION ok(x : memrange) : memrange;
    BEGIN
    IF x < 1 THEN
        point('F','null address being referenced ');
    ok := x
    END; (* ok *)

    FUNCTION o(x : memrange) : standardident;
    BEGIN o := m[ok(x)].op END;

    FUNCTION b(x : memrange) : boolean;
    BEGIN b := m[ok(x)].val > 0 END;

    FUNCTION i(x : memrange) : integer;
    BEGIN
    WITH m[ok(x)] DO
        IF op = integer_ THEN i := val ELSE
            BEGIN
            point('R','integer value required        ');
            GOTO 10
            END
    END; (* i *)

    FUNCTION l(x : memrange) : memrange;
    BEGIN
    WITH m[ok(x)] DO
        IF op = list_ THEN l := val ELSE
            BEGIN
            point('R','list value required           ');
            GOTO 10
            END
    END;

    FUNCTION n(x : memrange) : memrange;
    BEGIN
    WITH m[ok(x)] DO
        IF nxt >= 0 THEN n := nxt ELSE
            BEGIN
            point('R','negative next value           ');
            GOTO 10
            END
    END; (* n *)

    FUNCTION v(x : memrange) : integer;
    BEGIN v := m[ok(x)].val END;

BEGIN (* joy *)
nod := node;
WHILE nod > 0 DO
    WITH m[nod] DO
        BEGIN
        IF writelisting > 3 THEN
            BEGIN
            writeident('joy:            '); putch(' ');
            writefactor(nod,true)
            END;
        IF writelisting > 4 THEN
            BEGIN
            writeident('stack:          '); putch(' ');
            writeterm(s,true);
            writeident('dump:           '); putch(' ');
            writeterm(dump,true);
            END;
        last_op_executed := op;
        CASE op OF
            nothing_,char_,integer_,list_ : s := kons(op,val,s);
            true_,false_ : s := kons(boolean_,ord(op = true_),s);
            pop_ : s := n(s);
            dup_ : s := kons(o(s),v(s),s);
            swap_ :
                s := kons(o(n(s)),v(n(s)),
                          kons(o(s),v(s),n(n(s))) );
            stack_ : s := kons(list_,s,s);
            unstack_ : s := l(s);
                                                (* OPERATIONS: *)
            not_ : s := kons(boolean_,ord(NOT b(s)),n(s));
            mul_ : binary(integer_,i(n(s)) * i(s));
            add_ : binary(integer_,i(n(s)) + i(s));
            sub_ : binary(integer_,i(n(s)) - i(s));
            div_ : binary(integer_,i(n(s)) DIV i(s));
            and_ : binary(boolean_,ord(b(n(s)) AND b(s)));
            or_  : binary(boolean_,ord(b(n(s)) OR b(s)));
            les_ : IF o(s) = lib_
                     THEN binary(boolean_,ord(table[v(n(s))].alf
                                            < table[v(s)].alf))
                     ELSE binary(boolean_,ord(v(n(s)) < v(s)));
            eql_ : binary(boolean_,ord(v(n(s)) = v(s)));
            sty_ : binary(boolean_,ord(o(n(s)) = o(s)));
            cns_ :
                IF o(n(s)) = nothing_
                    THEN s := kons(list_,l(s),n(n(s)))
                    ELSE s := kons(list_,
                                   kons(o(n(s)),v(n(s)),v(s)),
                                   n(n(s)));
            uncns_ :
                IF m[s].val = 0
                    THEN s := kons(list_,0,
                                   kons(nothing_,ord(nothing_),
                                        n(s)))
                    ELSE s := kons(list_,n(l(s)),
                                   kons(o(l(s)),m[l(s)].val,
                                        n(s)));
            sel_ :
                BEGIN
                temp1 := l(s);
                WHILE o(l(temp1)) <> o(n(s))
                    DO temp1 := n(temp1);
                s := kons(list_,n(l(temp1)),n(s))
                END;
            index_ :
                IF v(n(s)) < 1 THEN
                    s := kons(o(l(s)),v(l(s)),n(n(s)))
                ELSE
                    s := kons(o(n(l(s))),v(n(l(s))),n(n(s)));
            body_ : s := kons(list_,table[v(s)].adr,n(s));
            put_ : BEGIN writefactor(s,false); s := m[s].nxt END;
            get_ :
                BEGIN
                getsym; readfactor(temp1);
                s := kons(o(temp1),v(temp1),s)
                END;
                                                (* COMBINATORS: *)
            i_ :
                BEGIN
                temp1 := s;
                s := n(s);
                joy(l(temp1))
                END;
            dip_ :
                BEGIN
                dump := kons(o(n(s)),v(n(s)),dump);
                dump := kons(list_,l(s),dump);
                s := n(n(s));
                joy(l(dump));
                dump := n(dump);
                s := kons(o(dump),v(dump),s);
                dump := n(dump);
                END;
            step_ :
                BEGIN
                dump := kons(o(s),l(s),dump);
                dump := kons(o(n(s)),l(n(s)),dump);
                temp1 := l(s);
                temp2 := l(n(s));
                s := n(n(s));
                WHILE temp2 > 0 DO
                    WITH m[temp2] DO
                        BEGIN
                        s := kons(op,val,s);
                        joy(temp1);
                        temp2 := nxt
                        END;
                dump := n(n(dump))
                END;
            lib_ : joy(table[val].adr);
            OTHERWISE
                point('F','internal error in interpreter ')
            END; (* CASE *)
        stat_ops := stat_ops + 1;
        nod := nxt
        END; (* WITH, WHILE *)
stat_calls := stat_calls + 1
END; (* joy *)


```
