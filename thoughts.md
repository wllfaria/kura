# Remember

This is the BNF of what I figured out so far

```
Digit = "0".."9"
Letter = "a".."z" | "A".."Z"
    
Ident = ( Letter | "_" ) { Letter | "_" | Digit }

Ident
Ident<Ident>
Ident<Ident<Ident>>
&&Ident<&const Ident>


Type =
	Ident
	| Ident "<" Type ">"
	| "&" Type
	| "&const" Type
     
Function = "fun" Ident "(" ( Ident ":" Type ) { "," Ident ":" Type } ")" [ "=>" Type ] "{" ExprList "}"
	
ExprList = { Expr }

Expr = 
    BinOp
    | <TODO>

BinOp = Digit | BinOp InfixOp BinOp
    
InfixOp = "*" | "-" | "+" | "/"
```

Statement ::=
    

```rust
struct StructName {
    struct_field: usize
    another_struct_field: String,
}

let name = fun(abc i32, something_else ) -> i32 {}
fun name(some_argument i32, another_argument usize) -> i32 {}

fun main() {
    const fixed_array = [1, 2, 3, 4];
    // var typed_vector: vec<StructName>  = Vec()

    if fixed_array.find(3) {
        const my_struct StructName {
            struct_field = usize(10),
            another_struct_field = String("something"),
        };

        return my_struct;
    }

    return StructName {
        struct_field = 0,
        another_struct_field = String(""),
    }
}
```
