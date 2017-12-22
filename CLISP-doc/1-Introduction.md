## 1.1 特殊符号(Special Symbols) [sec_1-4-1-3]

交互流中的定界符:

UNIX
在一行的开头输入 Control+D 
Win32
在 Enter 后输入 Control+Z
这个定界符对程序来说是不可见的; 用不着去为 #\^D 或者 #\^Z 测试 - 使用 READ-CHAR-NO-HANG 去为 end-of-stream 检测.

一个新的行字符可以通过按 Enter 键来输入.

也见 Section 21.13, “Function CLEAR-INPUT”.

## 1.2 错误术语(Error Terminology) [sec_1-4-2]

安全性设置是被解释的代码所忽略的; 因此在使用了术语 “should signal an error” 的地方, 一个 ERROR 会被发出来. 关于编译代码的安全性见 Section 3.3.4, “Declaration SAFETY” . ;;;???

 

## 1.3 包 “COMMON-LISP” 中的符号 [sec_1-9]

所有 [ANSI CL standard]  中指定的978个符号在 “COMMON-LISP” 包中实现了.