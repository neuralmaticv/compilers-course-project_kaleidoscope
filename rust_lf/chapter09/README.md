
# Chapter 9: Debug Information

[Link for tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl09.html)  

```bash
clang++ -Xlinker --export-dynamic -g main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main
```
