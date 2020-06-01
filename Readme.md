## CParser

This is an experimental project to build a C parser/analyzer with Ada.

Currently, this project parses a C input file and generates an AST, which is then printed to the console. We then take the AST, and convert it back to C (this part of the program is called the unwriter). The purpose of the unwriter is mainly for testing purposes. If the generated abstract tree is correct and unwritten correctly, it should compile and produce the same output as the original. The test script does that; in both cases, GCC is used.

The goal of this project is to be able to parse most of C. After the inital parsing is done, I want to explore adding some optimization features. Eventually, I would like to add a backend or connect it with one of my existing backends (ie, the extcc project).

### Why Ada?

This project serves a dual-purpose to learn Ada and explore the use of the language for compilers. Ada is a very safe, robust language, and I think such a language would be well-suited for compilers. In my other compiler projects (most written in C++), I often have the problem of run-time errors, especially segmentation faults. I've explored the use of Haskell, and I think it probably would be well suited, but there are a few issues I have with it in terms of parsing.

Other than readability, one of my favorite things with Ada so far is the ability to create subprograms. This feature is really nice, and overall I think it results in cleaner code.
