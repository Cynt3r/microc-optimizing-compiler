# microC optimizing compiler [![pipeline status](https://gitlab.fit.cvut.cz/kralva10/microc-optimizing-compiler/badges/main/pipeline.svg)](https://gitlab.fit.cvut.cz/kralva10/microc-optimizing-compiler/commits/main)
Ahead-of-time compiler for microC language that is used in the program analysis course (NI-APR).
Compiler serves as a demonstration of usage of various static analyses covered in the first half of the NI-APR course.
MicroC language is compiled into Linux native x86-64 assembly.
Note that this is the "public" version of the project that does not include implemented analyses.

#### Table of contents
- [Supported microC language subsets](#supported_lang_subsets)
- [Supported analyses](#supported_analyses)
- [How to use](#how_to_use)
- [How to compile and run code](#how_to_run)
- [Example of generated assembly code](#example)

<a name="supported_lang_subsets"/>

## Supported microC language subsets
- [x] microCVar (basic microC constructs with no control flow)
- [x] microCIf (microCVar with if/else statements)
- [x] microCWhile (microCIf with while statements)
- [x] microCFun (microCWhile with function calls)
- [x] microCRec (microCFun with records)
- [x] microCArr (microCRec with arrays)
- [x] microC (entire microC language)

<a name="supported_analyses"/>

## Supported analyses
- [x] Semantic analysis
- [x] Type analysis
- [x] Sign analysis
- [x] Constant propagation analysis
- [x] Live variable analysis
- [x] Available expressions analysis

<a name="how_to_use"/>

## How to use
Before running the compiler you will have to implement `AnalysisHandlerInterface` trait which defines methods for retrieving analysis results.
Minimum required analyses are type and semantic analyses (which are essential for the compiler).
Every other analysis is optional and not required.

If you have implemented some analysis and you want to provide them to the compiler, just override corresponding methods of `AnalysisHandlerInterface`.
For example if you've implemented sign analysis in addition to those required analyses, override its method like so:

```scala
object MyAnalysisHandler extends AnalysisHandlerInterface {
    override def getTypes(program: Program): Types = {
        new TypeAnalysis(program, NoopLogger)(getDeclarations(program)).analyze() //your implementation of a type analysis (required)
    }

    override def getDeclarations(program: Program): Declarations = {
        new SemanticAnalysis(program).analyze() //your implementation of a semantic analysis (required)
    }
    
    override def getSigns(cfg: ProgramCfg)(implicit declarations: Declarations): Option[Signs] = {
        val signs = new SignAnalysis(cfg).analyze() //your implementation of a sign analysis
        Some(signs)
    }
}
```

Now the compiler will be able to perform optimizations that utilize results of a sign analysis.

Finally to run the compiler simply create instance of `Compiler` by providing your implementation of `AnalysisHandlerInterface`, compiled language and syntax of x86 assembly.
Then call the `compile()` method with microC code that you want to compile:

```scala
val compiler = new Compiler(MyAnalysisHandler, Language.microCVar, X86Syntax.NASM)
val assembly = compiler.compile(code, optimize = true)
```


<a name="how_to_run"/>

## How to compile and run code
### Prerequisites
Linux OS is required, since the compiler produces Linux-native x86-64 assembly.
- [nasm](https://linux.die.net/man/1/nasm) - NASM assembler for assembling of the assembly code produced by the compiler
    ```sh
    apt-get install nasm
    ```
- [ld](https://linux.die.net/man/1/ld) - GNU linker required for linking and compiling object files produced by the `nasm` command
    ```sh
    apt-get install binutils
    apt-get install binutils-x86-64-linux-gnu
    ```
- [libc](https://man7.org/linux/man-pages/man7/libc.7.html) - standard C library
    ```sh
    apt-get install libc6-dev
    ```
### Run and compile
1. To run the compiler you can either (note that the minimum required version of JDK is 11):
    - Use [IntelliJ Idea with a Scala plugin](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html)
    - Or use the prebuild jar file located in the root of the project that uses example implementations of the analyses:
        ```sh
        java -jar microc-compiler.jar MICROC_FILE [-o] > program.asm
        ```
        where `MICROC_FILE` is a name of file with a microC source code and `-o` is an optional flag that decides whether optimizations should be performed.
    - Or use the building tool *sbt* by running the following command from the folder `microc-compiler/`:
        ```sh
        sbt run MICROC_FILE [-o] > program.asm
        ```
2. Assemble the assembly code produced by the compiler:
    ```sh
    nasm -felf64 program.asm
    ```
3. Link and compile:
    ```sh
    ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o program program.o
    ```
4. Run the program:
    ```sh
    ./program
    ```

<a name="example"/>

## Example of generated assembly code
Following examples are compiled from this microC source code:
```python
main() {
  var x, y;
  x = 42;
  while (x) {
    if (input) {
      y = 5;
    } else {
      y = 10;
    }
    x = 0 / y;
  }
  return y;
}
```

Example of an assembly code produced by the compiler (no optimizations):
```nasm
; assemble, link and run with:
; nasm -felf64 test.asm && ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o test test.o && ./test
section .data
printfFmt db "%ld", 10, 0 ; format string for printf()
scanfFmt db "%ld", 0 ; format string for scanf()
scanfRes times 4 db 0 ; address for scanf() result
__main: dd 0,0,0,0

section .text
global _start
extern printf
extern scanf
extern malloc
extern exit

_start:
  mov rax, main
  mov [__main], rax
  call main
  call exit
main:
  push rbp                ; Save previous base pointer
  mov rbp, rsp            ; Update base pointer
  sub rsp, 16             ; Allocate frame space on stack
bb_1:
  mov eax, 42
  mov [rbp - 8], eax
  jmp guard_2
guard_2:
  mov eax, [rbp - 8]
  cmp eax, 0
  je finally_2
  jmp body_2
body_2:
  mov rsi, scanfRes       ; Address of scanned integer
  mov rdi, scanfFmt       ; Format string for scanf()
  call scanf              ; Call scanf()
  cmp eax, 1
  je body_2_notEof_1
  mov eax, -1
  jmp body_2_afterInput_1
body_2_notEof_1:
  mov eax, [scanfRes]
body_2_afterInput_1:
  cmp eax, 0
  je else_3
  jmp then_3
then_3:
  mov eax, 5
  mov [rbp - 16], eax
  jmp finally_3
else_3:
  mov eax, 10
  mov [rbp - 16], eax
  jmp finally_3
finally_3:
  mov eax, [rbp - 16]
  mov edi, 0
  mov r10d, eax
  push rdx
  mov edx, 0
  mov eax, edi
  idiv r10d
  pop rdx
  mov [rbp - 8], eax
  jmp guard_2
finally_2:
  mov eax, [rbp - 16]
  mov eax, eax
  mov rsp, rbp            ; Reset stack pointer
  pop rbp                 ; Restore base pointer
  ret
```

Example of an assembly code produced by the compiler (with optimizations):
```nasm
; assemble, link and run with:
; nasm -felf64 test.asm && ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o test test.o && ./test
section .data
printfFmt db "%ld", 10, 0 ; format string for printf()
scanfFmt db "%ld", 0 ; format string for scanf()
scanfRes times 4 db 0 ; address for scanf() result
__main: dd 0,0,0,0

section .text
global _start
extern printf
extern scanf
extern malloc
extern exit

_start:
  mov rax, main
  mov [__main], rax
  call main
  call exit
main:
  push rbp              ; Save previous base pointer
  mov rbp, rsp          ; Update base pointer
  sub rsp, 16           ; Allocate frame space on stack
bb_1:
  mov rsi, scanfRes     ; Address of scanned integer
  mov rdi, scanfFmt     ; Format string for scanf()
  call scanf            ; Call scanf()
  cmp eax, 1
  je bb_1_notEof_1
  mov eax, -1
  jmp bb_1_afterInput_1
bb_1_notEof_1:
  mov eax, [scanfRes]
bb_1_afterInput_1:
  cmp eax, 0
  je else_2
then_2:
  mov eax, 5
  mov [rbp - 8], eax
  jmp finally_2
else_2:
  mov eax, 10
  mov [rbp - 8], eax
finally_2:
  mov eax, [rbp - 8]
  mov rsp, rbp          ; Reset stack pointer
  pop rbp               ; Restore base pointer
  ret
```
