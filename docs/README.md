<h1 style="text-indent: -9999px; height: 0;">Oden</h1>
<div class="logo" style="text-align: center;">
	<img src="logo.png" alt="Oden Logo" style="margin: 0 auto; width: 100%; max-width: 300px; padding-bottom: 40px;"/>
</div>

```scheme
(pkg fibonacci)

(define (fib [n : int])
  (if (== n 1)
    0
    (if (== n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))
```

**Welcome to the Oden Language Documentation!**

Oden is a LISP-inspired language with static typing that compiles
to native code using Go. The current implementation is working draft
and **should be considered highly experimental**.

[The source is available on GitHub.](https://github.com/oden-lang/oden)
