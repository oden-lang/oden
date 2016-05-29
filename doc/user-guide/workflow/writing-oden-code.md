# Writing Oden Code

In Oden the following rules apply:

* Every source file corresponds to a single package.
* Source files must begin with the `package` declaration. The declaration
  specifies the complete qualified name, e.g. `package a/b/c`.
* Source files may use zero or more `import` declarations after the `package`
  declaration.
* Source files may, after `package` and any `import` declarations, define zero
  or more functions and values.
* Functions must be defined before first use (this restriction will be lifted
  as soon as possible).

## Example

<div class="playground-runnable">
<pre><code class="lang-go">// package declaration
package main

// function definitions can use the function definition shorthand
// and type signatures
shout : string -> string
shout(s) = s ++ "!"

// value definition
result = shout("hello, world")

// main function definition, must have type: -> unit
main() = println(result)
</code></pre>
</div>
