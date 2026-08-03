经过一定量的合法代码检验，目前已实现从IR到RV32I汇编语言的编译，代码具体功能如下：

对于 lib/codegen.ml 部分：

1. compute_offsets 算法为函数内部的所有局部变量、IR 临时变量（Temp）以及超过 8 个的溢出参数分配固定的栈槽偏移量；

2. 遵循 RISC-V 的 16 字节栈对齐 要求，动态计算每个函数的 framesize，并在函数序言与结语中安全地恢复栈指针（sp）、帧指针（fp）及返回地址（ra）；

3. 严格遵循 Naive opproach 机制，将 a0-a7 中的输入参数立即转存到本地栈槽，确保了 Naive 变量访问机制下寄存器不会被后续的嵌套调用破坏；

4. 针对 RV32I 基础整数指令集不包含乘、除、模运算的硬件限制，将其标准化翻译为内联函数，其余使用了RV32I的标准指令与伪指令；

对于 bin/main.ml 部分：

1. 移除了原本仅作为调试输出的中间代码 Dump 逻辑（Lib.Ir.dump_ir），将其替换为后端生成器主入口 Lib.Codegen.generate_riscv 的调用；

2. 当源程序通过语义检查并成功生成 IR 后，编译器现在会直接能通过自定义标准输出打印出结构完整、能够直接被 GNU 汇编器（as 或 gcc）编译的目标 RV32I 汇编文件（.s）。

如何执行：
bash
dune build
cat test/test.c(源代码相对地址) | dune exec ./_build/default/bin/main.exe(程序相对地址) > test.s(自定义标准输出)
执行后即可在 bash 打开位置生成 test.s 汇编文件。