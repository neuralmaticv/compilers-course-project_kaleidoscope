## Chapter 4: Adding JIT and Optimizer Support

[Link for tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl04.html)  

```bash
clang++ -g main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main
```
