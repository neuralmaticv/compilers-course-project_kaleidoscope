
# Chapter 7: Extending the Language: Mutable Variables 

[Link for tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl07.html)  

```bash
clang++ -Xlinker --export-dynamic -g main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main
```

### Test examples for Rust_LF
 
```bash
use putchard(c:f32);

let mut i = 48.0 in for j in 0...10 {putchard(i); i=i+1;};

fn fib(x:f32) {if x < 3.0 {1.0;}else{fib(x-1.0)+fib(x-2.0);};}



// Mandelbrot set [Doesn't work in this version]
use putchard(c:f32);

fn binary : 1 (x:f32, y:f32) {0;}

fn binary> 10 (LHS:f32, RHS:f32) {RHS < LHS;}


fn printdensity(d:f32) {if d > 8.0 {putchard(32);} else {putchard(42);};}

fn mandleconverger(real:f32, imag:f32, iters:f32, creal:f32, cimag:f32) { if iters > 255.0 | (real*real + imag*imag > 4.0) {iters;} else { mandleconverger(real*real - imag*imag + creal, 2*real*imag + cimag, iters+1, creal, cimag);} ;}



fn mandleconverge(real:f32, imag:f32) {mandleconverger(real, imag, 0, real, imag);}

fn mandlehelper(xmin:f32, xmax:f32, xstep:f32, ymin:f32, ymax:f32, ystep:f32) {
    let yst = ystep, xst = xstep in for y in ymin...ymax {
        for x in xmin...xmax {
            printdensity(mandleconverge(x, y));
            x = x + xst;
        };
        y = y + yst;
        putchard(10);
    };
}

fn mandel(realstart:f32, imagstart:f32, realmag:f32, imagmag:f32) {
    mandlehelper(realstart, realstart + realmag*78.0, realmag, imagstart, imagstart + imagmag*40.0, imagmag);
}

```


