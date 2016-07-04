lambdaI
========

An implementation of type inference algorithm in [1].  You can try it at <https://www.fos.kuis.kyoto-u.ac.jp/~nishida/lambdaI/>.

## Reference

[1] A. J. Kfoury and J. B. Wells.  Principality and Type Inference for Intersection Types Using Expansion Variables.  In Theoretical Computer Science, Vol.311, Issue 1-3, 2004.  pp.1-70.

## Required OCaml tools & libraries

- omake
- menhir
- ppx_deriving_yojson
- js_of_ocaml

## Usage

Simply type the following command in the project root directory.

```
$ omake
```

Then you have an interpreter file `lambdaI` and be able to use it as follows.

```
$ ./lambdaI
```

The language syntax is so simple:

- variable `x`, `y`, etc.\ lower case identifiers
- function `fun x -> e`
- application `e1 e2`

Here are some interaction examples:

```
> fun x -> x;;
Skel:<ABS_I:{} |- (fun x -> x) : ('a1 -> 'a1)
            <VAR:{x : 'a1} |- x : 'a1>>
Cstr:{}
Unifier:{||}
Deriv:<ABS_I:{} |- (fun x -> x) : ('a11 -> 'a11)
             <VAR:{x : 'a11} |- x : 'a11>>
Type:('a1 -> 'a1)
> fun x -> x x;;
Skel:<ABS_I:{} |- (fun x -> (x x)) : (('a3 /\ (F1 'a4)) -> 'a5)
            <APP:{x : ('a3 /\
                  (F1 'a4))} |- (x x) : 'a5
                 <VAR:{x : 'a3} |- x : 'a3>
                 <F:{x : (F1 'a4)} |- x : (F1 'a4)
                    <VAR:{x : 'a4} |- x : 'a4>>>>
Cstr:{'a3 = ((F1 'a4) -> 'a5)}
Unifier:{|'a3 := ((F1 'a4) -> 'a5)|}
Deriv:<ABS_I:{} |- (fun x -> (x x)) : ((((F4 'a14) -> 'a15) /\ (F4 'a14)) ->
             'a15)
             <APP:{x : (((F4 'a14) ->
                   'a15) /\
                   (F4 'a14))} |- (x x) : 'a15
                  <VAR:{x : ((F4 'a14) ->
                        'a15)} |- x : ((F4 'a14) -> 'a15)>
                  <F4:{x : (F4 'a14)} |- x : (F4 'a14)
                      <VAR:{x : 'a14} |- x : 'a14>>>>
Type:((((F1 'a4) -> 'a5) /\ (F1 'a4)) -> 'a5)
>
```

where `Skel` corresponds to **skeleton** in the paper, `Cstr` corresponds to
**constraints**, `Unifier` is **substitution** which solves the constraints, and
`Type` is the inferred type.

## Web interface

You need `npm` command in addition to requirements above.  The following
commands build a web interface.

```
$ npm install --dev
$ omake web
```

You can access the interface from `public/index.html`