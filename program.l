// { if 2 = 2 then a := 1 else a := 2 end; { a := 15  ; a := 7 } }

// while Not (varA = 10) do varA := varA + 1

//var x int := 0

{ var a int := 1;
{ var b int := 2;
{ proc soma ( a :T int, b :T int, ) {
    a := a + b
}
{ call soma ( 1, 2 ); } } } }