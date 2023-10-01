
# Chapter 7: Extending the Language: Mutable Variables 

[Link for tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl07.html)  

```bash
clang++ -Xlinker --export-dynamic -g new_main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main
```
