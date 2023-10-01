## Chapter 8: Compiling to Object Files

[Link for tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html)  

```bash
clang++ -Xlinker --export-dynamic -g -O3 main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o main
```
