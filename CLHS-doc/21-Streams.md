# 21 流

> * 21.1 [流的概念](#StreamConcepts)
> * 21.2 [流的字典](#TheStreamsDictionary)

## 21.1 <span id="StreamConcepts">流的概念</span>

> * 21.1.1 [流的介绍](#IntroductionStreams)
> * 21.1.2 [流变量](#StreamVariables)
> * 21.1.3 [给标准函数的流参数](#StreamArgStandFun)
> * 21.1.4 [复合流上的限制](#RestrictCompositeStreams)

### 21.1.1 <span id="IntroductionStreams">流的介绍</span>

流是一个对象, 它可以与输入或输出函数一起使用, 以确定该操作的字符或字节的适当源或接收器. 一个字符流是一个字符的源或接收器. 一个二进制流是一个字节的源或接收器.

一些操作符可以在任何种类的流上执行; 下一段中提供了一个标准操作符的列表, 这些操作可能对任何类型的流都有用

    close                 stream-element-type  
    input-stream-p        streamp              
    interactive-stream-p  with-open-stream     
    output-stream-p                            

    Figure 21-1. 一些一般用途的流操作

其他操作只对特定的流类型有意义. 比如, read-char 只定义给字符流而 read-byte 只定义给二进制流.

> * 21.1.1.1 [抽象的流分类](#AbstractClassStreams1)
> * 21.1.1.2 [抽象的流分类](#AbstractClassStreams2)
> * 21.1.1.3 [流的其他子类](#OtherSubclassesStream)

#### 21.1.1.1 <span id="AbstractClassStreams1">抽象的流分类</span>

##### 21.1.1.1.1 输入, 输出, 和双向流

一个流, 不管是字符流或二进制流, 都可以是一个输入流 (数据源), 一个输出流 (数据的接收器), 两者都是, 或者 (比如, 当 ":direction :probe" 给定给 open 时) 都不是.

下一段中展示了和输入流相关的操作符.

    clear-input  read-byte            read-from-string            
    listen       read-char            read-line                   
    peek-char    read-char-no-hang    read-preserving-whitespace  
    read         read-delimited-list  unread-char                 

    Figure 21-2. 和输入流相关的操作符.

下一段中展示了和输出流相关的操作符.

    clear-output   prin1            write            
    finish-output  prin1-to-string  write-byte       
    force-output   princ            write-char       
    format         princ-to-string  write-line       
    fresh-line     print            write-string     
    pprint         terpri           write-to-string  

    Figure 21-3. 和输出流相关的操作符.

一个同时为输入流和输出流的流被称为双向流. 见函数 input-stream-p 和 output-stream-p.

任何在 Figure 21-2 或 Figure 21-3 列出的操作符可以和双向流一起使用. 另外, 下一段列出了与双向流相关的操作符列表.

    y-or-n-p  yes-or-no-p    

    Figure 21-4. 和双向流相关的操作符. 


##### 21.1.1.1.2 打开和关闭的流

流可以使打开的或关闭的.

除非被显式指定, 否则创建和返回流的操作返回的是打开的流.

关闭流的操作标志着它作为数据来源或接收器的结束, 允许实现收回其内部数据结构, 并释放在打开时可能被流锁定的任何外部资源.

除非被显式指定, 否则当一个关闭的流被用于调用流时后果是未定义的.

对于一个关闭的流, 把流强制转换为路径名是允许的; 在一些情况下, 例如在一个真实名字计算中, 对于一个打开的流和同一流被关闭后的结果是不同的. 

##### 21.1.1.1.3 Interactive Streams

交互式流是可以在上面执行交互式查询的一种流.

一个交互式流的准确定义是具体实现定义的, 并且可能依赖于底层操作系统. 实现可能选择用作识别交互流的特征的东西的一些示例包括:

* 这个流与一个人 (或同等的) 连接在一起, 程序可以提示信息并期望根据提示接收不同的输入 The stream is connected to a person (or equivalent) in such a way that the program can prompt for information and expect to receive different input depending on the prompt.<!-- TODO 待校验 -->

* 该程序期望提示输入和支持 "正常的输入编辑".

* read-char 可能等待用户在返回之前去输入一些东西而不是立即返回一个字符或 end-of-file.

将一些流划分为交互式流的一般意图是允许它们与包含批处理 (或后台或命令文件) 输入的流区分开来. 批处理流的输出通常被丢弃或保存以供以后查看, 因此对此类流的交互式查询可能没有预期的效果.

终端 I/O 可能是也可能不是一个交互式流. 

#### 21.1.1.2 <span id="AbstractClassStreams2">抽象的流分类</span>

##### 21.1.1.2.1 文件流

一些流, 称为文件流, 提供了对文件的访问. 一个 file-stream 类的对象被用来表示一个文件流.

打开一个文件的基础操作是 open, 它通常返回一个文件流 (详情见它的目录条目). 关闭一个流的基础操作是 close. 宏 with-open-file 是用来表达在给定代码主体的持续时间内打开文件的常用习惯，并确保在退出该主体时关闭那个产生的流.

#### 21.1.1.3 <span id="OtherSubclassesStream">流的其他子类</span>

类 stream 有许多由这个规范定义的子类. 下面这段展示了关于这些子类的一些信息.

    类                  相关操作符             
    broadcast-stream     make-broadcast-stream         
                        broadcast-stream-streams      
    concatenated-stream  make-concatenated-stream      
                        concatenated-stream-streams   
    echo-stream          make-echo-stream              
                        echo-stream-input-stream      
                        echo-stream-output-stream     
    string-stream        make-string-input-stream      
                        with-input-from-string        
                        make-string-output-stream     
                        with-output-to-string         
                        get-output-stream-string      
    synonym-stream       make-synonym-stream           
                        synonym-stream-symbol         
    two-way-stream       make-two-way-stream           
                        two-way-stream-input-stream   
                        two-way-stream-output-stream  

    Figure 21-5. 和特定的流相关的已定义的名字

### 21.1.2 <span id="StreamVariables">流变量</span>

值必须是流的变量有时称为流变量.

特定的流变量被这个规范定义为在没有指定特定流的情况下, 作为输入或输出的适当来源. 下一个段中会出现一个完整的标准流变量列表. 如果任何时候这些变量的任意一个的值不是一个打开的流, 后果是未定义的.

    词汇术语          变量名      
    debug I/O        *debug-io*         
    error output     *error-output*     
    query I/O        *query-io*         
    standard input   *standard-input*   
    standard output  *standard-output*  
    terminal I/O     *terminal-io*      
    trace output     *trace-output*     

    Figure 21-6. 标准流变量

注意, 按照惯例, 标准流变量如果一定式输入流, 那么就以 "-input*" 结尾, 如果一定是输出流那么就以 "-output*" 结尾, 如果一定是双向流就以 "-io*" 结尾.

用户程序可以赋值或绑定任何标准流变量除了 \*terminal-io\*. 


### 21.1.3 <span id="StreamArgStandFun">给标准函数的流参数</span>

下面这段中的操作符接受打开或关闭的流作为流参数.

    broadcast-stream-streams     file-author       pathnamep                     
    close                        file-namestring   probe-file                    
    compile-file                 file-write-date   rename-file                   
    compile-file-pathname        host-namestring   streamp                       
    concatenated-stream-streams  load              synonym-stream-symbol         
    delete-file                  logical-pathname  translate-logical-pathname    
    directory                    merge-pathnames   translate-pathname            
    directory-namestring         namestring        truename                      
    dribble                      open              two-way-stream-input-stream   
    echo-stream-input-stream     open-stream-p     two-way-stream-output-stream  
    echo-stream-ouput-stream     parse-namestring  wild-pathname-p               
    ed                           pathname          with-open-file                
    enough-namestring            pathname-match-p                                

    Figure 21-7. 接受打开或关闭的流的操作符

下面这段中的操作符只接受打开的流作为流参数.

    clear-input               output-stream-p          read-char-no-hang           
    clear-output              peek-char                read-delimited-list         
    file-length               pprint                   read-line                   
    file-position             pprint-fill              read-preserving-whitespace  
    file-string-length        pprint-indent            stream-element-type         
    finish-output             pprint-linear            stream-external-format      
    force-output              pprint-logical-block     terpri                      
    format                    pprint-newline           unread-char                 
    fresh-line                pprint-tab               with-open-stream            
    get-output-stream-string  pprint-tabular           write                       
    input-stream-p            prin1                    write-byte                  
    interactive-stream-p      princ                    write-char                  
    listen                    print                    write-line                  
    make-broadcast-stream     print-object             write-string                
    make-concatenated-stream  print-unreadable-object  y-or-n-p                    
    make-echo-stream          read                     yes-or-no-p                 
    make-synonym-stream       read-byte                                            
    make-two-way-stream       read-char                                            

    Figure 21-8. 只接受打开的流的操作符

### 21.1.4 <span id="RestrictCompositeStreams">复合流上的限制</span>

如果一个复合流的任意成员在这个流被关闭之前关闭, 那么后果是未定义的.

如果 synonym-stream 流符号从它被创建到它被关闭这段时间都没有被绑定到一个打开的流, 那么后果是未定义的. 

## 21.2 <span id="TheStreamsDictionary">流的字典</span>

> * [系统类 STREAM](#SC-STREAM)
> * [系统类 BROADCAST-STREAM](#SC-BROADCAST-STREAM)
> * [系统类 CONCATENATED-STREAM](#SC-CONCATENATED-STREAM)
> * [系统类 ECHO-STREAM](#SC-ECHO-STREAM)
> * [系统类 FILE-STREAM](#SC-FILE-STREAM)
> * [系统类 STRING-STREAM](#SC-STRING-STREAM)
> * [系统类 SYNONYM-STREAM](#SC-SYNONYM-STREAM)
> * [系统类 TWO-WAY-STREAM](#SC-TWO-WAY-STREAM)
> * [函数 INPUT-STREAM-P, OUTPUT-STREAM-P](#F-INPUT-AND-OUTPUT-STREAM-P)
> * [函数 INTERACTIVE-STREAM-P](#F-INTERACTIVE-STREAM-P)
> * [函数 OPEN-STREAM-P](#F-OPEN-STREAM-P)
> * [函数 STREAM-ELEMENT-TYPE](#F-STREAM-ELEMENT-TYPE)
> * [函数 STREAMP](#F-STREAMP)
> * [函数 READ-BYTE](#F-READ-BYTE)
> * [函数 WRITE-BYTE](#F-WRITE-BYTE)
> * [函数 PEEK-CHAR](#F-PEEK-CHAR)
> * [函数 READ-CHAR](#F-READ-CHAR)
> * [函数 READ-CHAR-NO-HANG](#F-READ-CHAR-NO-HANG)
> * [函数 TERPRI, FRESH-LINE](#F-TERPRI-FRESH-LINE)
> * [函数 UNREAD-CHAR](#F-UNREAD-CHAR)
> * [函数 WRITE-CHAR](#F-WRITE-CHAR)
> * [函数 READ-LINE](#F-READ-LINE)
> * [函数 WRITE-STRING, WRITE-LINE](#F-WRITE-STRING-WRITE-LINE)
> * [函数 READ-SEQUENCE](#F-READ-SEQUENCE)
> * [函数 WRITE-SEQUENCE](#F-WRITE-SEQUENCE)
> * [函数 FILE-LENGTH](#F-FILE-LENGTH)
> * [函数 FILE-POSITION](#F-FILE-POSITION)
> * [函数 FILE-STRING-LENGTH](#F-FILE-STRING-LENGTH)
> * [函数 OPEN](#F-OPEN)
> * [函数 STREAM-EXTERNAL-FORMAT](#F-STREAM-EXTERNAL-FORMAT)
> * [宏 WITH-OPEN-FILE](#M-WITH-OPEN-FILE)
> * [函数 CLOSE](#F-CLOSE)
> * [宏 WITH-OPEN-STREAM](#M-WITH-OPEN-STREAM)
> * [函数 LISTEN](#F-LISTEN)
> * [函数 CLEAR-INPUT](#F-CLEAR-INPUT)
> * [函数 FINISH-OUTPUT, FORCE-OUTPUT, CLEAR-OUTPUT](#F-FINISH-AND-FORCE-AND-CLEAR-OUTPUT)
> * [函数 Y-OR-N-P, YES-OR-NO-P](#F-Y-OR-N-P-YES-OR-NO-P)
> * [函数 MAKE-SYNONYM-STREAM](#F-MAKE-SYNONYM-STREAM)
> * [函数 SYNONYM-STREAM-SYMBOL](#F-SYNONYM-STREAM-SYMBOL)
> * [函数 BROADCAST-STREAM-STREAMS](#F-BROADCAST-STREAM-STREAMS)
> * [函数 MAKE-BROADCAST-STREAM](#F-MAKE-BROADCAST-STREAM)
> * [函数 MAKE-TWO-WAY-STREAM](#F-MAKE-TWO-WAY-STREAM)
> * [函数 TWO-WAY-STREAM-INPUT-STREAM, TWO-WAY-STREAM-OUTPUT-STREAM](#F-T-W-S-INPUT-AND-OUTPUT-STREAM)
> * [函数 ECHO-STREAM-INPUT-STREAM, ECHO-STREAM-OUTPUT-STREAM](#F-ECHO-STREAM-INPUT-AND-OUTPUT-STREAM)
> * [函数 MAKE-ECHO-STREAM](#F-MAKE-ECHO-STREAM)
> * [函数 CONCATENATED-STREAM-STREAMS](#F-CONCATENATED-STREAM-STREAMS)
> * [函数 MAKE-CONCATENATED-STREAM](#F-MAKE-CONCATENATED-STREAM)
> * [函数 GET-OUTPUT-STREAM-STRING](#F-GET-OUTPUT-STREAM-STRING)
> * [函数 MAKE-STRING-INPUT-STREAM](#F-MAKE-STRING-INPUT-STREAM)
> * [函数 MAKE-STRING-OUTPUT-STREAM](#F-MAKE-STRING-OUTPUT-STREAM)
> * [宏 WITH-INPUT-FROM-STRING](#M-WITH-INPUT-FROM-STRING)
> * [宏 WITH-OUTPUT-TO-STRING](#M-WITH-OUTPUT-TO-STRING)
> * [变量 *DEBUG-IO*, *ERROR-OUTPUT*, *QUERY-IO*, *STANDARD-INPUT*, *STANDARD-OUTPUT*, *TRACE-OUTPUT*](#V-IO-ALL)
> * [变量 *TERMINAL-IO*](#V-TERMINAL-IO)
> * [状况类型 STREAM-ERROR](#CT-STREAM-ERROR)
> * [函数 STREAM-ERROR-STREAM](#F-STREAM-ERROR-STREAM)
> * [状况类型 END-OF-FILE](#CT-END-OF-FILE)


### <span id="SC-STREAM">系统类 STREAM</span>

* 类优先级列表(Class Precedence List):

        stream, t

* 描述(Description):

        一个流式一个可以和一个输入或输出函数一起使用, 以确定该操作的字符或字节的适当源或接收器.

        关于更多完整信息, 见章节 21.1 (流的概念).

* 也见(See Also):

        章节 21.1 (流的概念), 章节 22.1.3.13 (Printing Other Objects), 章节 22 (Printer), 章节 23 (Reader) 

### <span id="SC-BROADCAST-STREAM">系统类 BROADCAST-STREAM</span>

* 类优先级列表(Class Precedence List):

        broadcast-stream, stream, t

* 描述(Description):

        一个广播流式一个输出流, 它和一个包含零个或多个输出流的集合关联, 这样一来发送到这个广播流的任何输出会作为输出传递到每一个关联的输出流上. (如果一个广播流没有成员流, 那么所有到广播流的输出会被废弃.)

        可以在一个广播流上执行的操作的集合是可应用到它关联的那些输出流的操作的交集.

        一些输出操作 (比如, fresh-line) 返回基于这个操作时那个流的状态的值. 由于这些值可能有别于这些成员流的每一个, 有必要具体描述它们的返回值:

        * stream-element-type 从最后一个成员流中返回这个值, 如果这里没有成员流就返回 t.

        * fresh-line returns 从最后一个成员流中返回这个值, 如果这里没有成员流就返回 nil.

        * 函数 file-length, file-position, file-string-length, 和 stream-external-format 从最后一个成员流来返回值; 如果这里没有成员流, file-length 和 file-position 返回 0, file-string-length 返回 1, 而 stream-external-format 返回 :default.

        * 函数 streamp 和 output-stream-p 对于广播流总是返回 true.

        * 函数 open-stream-p 检验这个广播流是否打开, 而不是它的成员流是否打开.

        * 函数 input-stream-p 和 interactive-stream-p 返回一个具体实现定义的, 广义 boolean 值.

        * 对于输入操作 clear-input, listen, peek-char, read-byte, read-char-no-hang, read-char, read-line, 和 unread-char, 如果所指示的操作被执行, 其后果是未定义的. 然而, 一个实现允许去定义这样一个行为作为一个依赖于具体实现的扩展.

        对于任何上面或这个文档的其他地方没有显式指定返回值的输出操作, 定义了这样一个操作返回的值式在它的最后一个成员流上执行这个操作产生的值; 在前面所有的流上执行这个操作产生的值会被丢弃. 如果这里没有成员流, 这个值是依赖于具体实现的. 

* 也见(See Also):

        broadcast-stream-streams, make-broadcast-stream 


### <span id="SC-CONCATENATED-STREAM">系统类 CONCATENATED-STREAM</span>

* 类优先级列表(Class Precedence List):

        concatenated-stream, stream, t

* 描述(Description):

        一个串联流是一个输入流, 它式一个包含零个或多个输入流的复合流, 这样, 可以从串联流中读取的数据序列与可以从每个组成流中读取的数据序列的连接相同.

        来自串联流的输入从第一个相关的输入流中获取, 直到它到达文件的末尾; 然后这个流会被废弃, 并且后续的输入取自于下一个输入流, 以此类推. 在相关输入流上的文件结束总是由串联流来管理---串联流的客户端只有在尝试从串联流中获取数据，但是它没有剩余的输入流来获取这些数据时才会看到文件的结束.

* 也见(See Also):

        concatenated-stream-streams, make-concatenated-stream 


### <span id="SC-ECHO-STREAM">系统类 ECHO-STREAM</span>

* 类优先级列表(Class Precedence List):

        echo-stream, stream, t

* 描述(Description):

        一个回音流是一个双向流, 它从一个关联的输入流中得到它的输入并且发送它的输出到一个关联的输出流.

        所有从输入流中获取的输入都与输出流相呼应. 输入是否在遇到后或者在从输入流中读取之后立即响应, 是依赖于具体实现的.

* 也见(See Also):

        echo-stream-input-stream, echo-stream-output-stream, make-echo-stream 


### <span id="SC-FILE-STREAM">系统类 FILE-STREAM</span>

* 类优先级列表(Class Precedence List):

        file-stream, stream, t

* 描述(Description):

        一个 file-stream 类型的对象是一个直接源和接收器都是一个文件的流. 这样一个流通过 open 和 with-open-file 显式创建, 以及通过像 load 这样的处理文件的函数隐式打开.

* 也见(See Also):

        load, open, with-open-file 


### <span id="SC-STRING-STREAM">系统类 STRING-STREAM</span>

* 类优先级列表(Class Precedence List):

        string-stream, stream, t

* 描述(Description):

        一个字符串流式一个读取输入自一个关联字符串或写入输出到一个关联字符串的流.

        一个字符串流的流元素类型总是为 character 类型的子类型.

* 也见(See Also):

        make-string-input-stream, make-string-output-stream, with-input-from-string, with-output-to-string 


### <span id="SC-SYNONYM-STREAM">系统类 SYNONYM-STREAM</span>

* 类优先级列表(Class Precedence List):

        synonym-stream, stream, t

* 描述(Description):

        一个是另一个流的别名的流, 它是一个动态变量的值, 这个变量的名字是这个同义流的同义流符号.

        任何在一个同义流上的操作会在一个流上被执行, 这个流是由那个同义流符号命名的动态变量的值. 如果那个变量的值将要改变, 或者如果那个变量的值将要被绑定, 那么这个流会操作在那个变量的新值上.

* 也见(See Also):

        make-synonym-stream, synonym-stream-symbol 


### <span id="SC-TWO-WAY-STREAM">系统类 TWO-WAY-STREAM</span>

* 类优先级列表(Class Precedence List):

        two-way-stream, stream, t

* 描述(Description):

        一个从一个关联输入流中接收它的输入并且发送它的输出到一个关联的输出流的双向复合流.

* 也见(See Also):

        make-two-way-stream, two-way-stream-input-stream, two-way-stream-output-stream 


### <span id="F-INPUT-AND-OUTPUT-STREAM-P">函数 INPUT-STREAM-P, OUTPUT-STREAM-P</span>

* 语法(Syntax):

        input-stream-p stream => generalized-boolean

        output-stream-p stream => generalized-boolean

* 参数和值(Arguments and Values):

        stream---一个流.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果流 stream 是一个输入流, input-stream-p 返回 true; 否则, 返回 false.

        如果流 stream 是一个输出流, output-stream-p 返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (input-stream-p *standard-input*) =>  true
    (input-stream-p *terminal-io*) =>  true
    (input-stream-p (make-string-output-stream)) =>  false

    (output-stream-p *standard-output*) =>  true
    (output-stream-p *terminal-io*) =>  true
    (output-stream-p (make-string-input-stream "jr")) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-INTERACTIVE-STREAM-P">函数 INTERACTIVE-STREAM-P</span>

* 语法(Syntax):

        interactive-stream-p stream => generalized-boolean

* 参数和值(Arguments and Values):

        stream---一个流.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果流 stream 是一个交互式流, 就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (when (> measured limit)
      (let ((error (round (* (- measured limit) 100)
                          limit)))
        (unless (if (interactive-stream-p *query-io*)
                    (yes-or-no-p "The frammis is out of tolerance by ~D%.~@
                                  Is it safe to proceed? " error)
                    (< error 15))  ;15% is acceptable
          (error "The frammis is out of tolerance by ~D%." error))))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 21.1 (流的概念)

* 注意(Notes): None. 


### <span id="F-OPEN-STREAM-P">函数 OPEN-STREAM-P</span>

* 语法(Syntax):

        open-stream-p stream => generalized-boolean

* 参数和值(Arguments and Values):

        stream---一个流.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果流 stream 是一个打开的流就返回 true; 否则, 返回 false.

        直到流被 close 显式关闭, 或者直到退出 with-output-to-string, with-open-file, with-input-from-string, 或 with-open-stream 表达式形式而被隐式关闭之前, 流都是打开的.

* 示例(Examples):

    ```LISP
    (open-stream-p *standard-input*) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        close.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-STREAM-ELEMENT-TYPE">函数 STREAM-ELEMENT-TYPE</span>

* 语法(Syntax):

        stream-element-type stream => typespec

* 参数和值(Arguments and Values):

        stream---一个流.
        typespec---一个类型指定符.

* 描述(Description):

        stream-element-type 返回可以从流 stream 中读到或写入到流 stream 中的对象的一个类型指定符.

        由 open 创建的流有着被约束为 integer 或一个 character 类型的子类型的元素类型.

* 示例(Examples):

    ```LISP
    ;; Note that the stream must accomodate at least the specified type,
    ;; but might accomodate other types.  Further note that even if it does
    ;; accomodate exactly the specified type, the type might be specified in
    ;; any of several ways.
    (with-open-file (s "test" :element-type '(integer 0 1)
                              :if-exists :error
                              :direction :output)
      (stream-element-type s))
    =>  INTEGER
    OR=>  (UNSIGNED-BYTE 16)
    OR=>  (UNSIGNED-BYTE 8)
    OR=>  BIT
    OR=>  (UNSIGNED-BYTE 1)
    OR=>  (INTEGER 0 1)
    OR=>  (INTEGER 0 (2))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None.

### <span id="F-STREAMP">函数 STREAMP</span>

* 语法(Syntax):

        streamp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 stream 类型就返回 true; 否则, 返回 false.

        如果对象 object 是一个流, streamp 不会被这个流是打开的还是关闭的所影响.

* 示例(Examples):

    ```LISP
    (streamp *terminal-io*) =>  true
    (streamp 1) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (streamp object) ==  (typep object 'stream)

### <span id="F-READ-BYTE">函数 READ-BYTE</span>

* 语法(Syntax):

        read-byte stream &optional eof-error-p eof-value => byte

* 参数和值(Arguments and Values):

        stream---一个二进制输入流.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        byte---一个整数, 或者是 eof-value.

* 描述(Description):

        read-byte 从流 stream 中读取并返回一个字节.

        如果到达了文件的末尾并且 eof-error-p 是 false, 那么返回这个 eof-value.

* 示例(Examples):

    ```LISP
    (with-open-file (s "temp-bytes" 
                        :direction :output
                        :element-type 'unsigned-byte)
        (write-byte 101 s)) =>  101
    (with-open-file (s "temp-bytes" :element-type 'unsigned-byte)
        (format t "~S ~S" (read-byte s) (read-byte s nil 'eof)))
    >>  101 EOF
    =>  NIL
    ```

* 副作用(Side Effects):

        修改流 stream.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

        如果 stream 不是一个二进制输入流, 那么应该发出一个 error 类型的错误.

        如果在流 stream 中没有剩余字节并且 eof-error-p 是 true, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also):

        read-char, read-sequence, write-byte

* 注意(Notes): None. 


### <span id="F-WRITE-BYTE">函数 WRITE-BYTE</span>

* 语法(Syntax):

        write-byte byte stream => byte

* 参数和值(Arguments and Values):

        byte---一个流的流元素类型的整数.
        stream---一个二进制输出流.

* 描述(Description):

        write-byte 向流 stream 写入一个字节 byte.

* 示例(Examples):

    ```LISP
    (with-open-file (s "temp-bytes" 
                        :direction :output
                        :element-type 'unsigned-byte)
        (write-byte 101 s)) =>  101
    ```

* 副作用(Side Effects):

        流 stream 会被修改.

* 受此影响(Affected By):

        流 stream 的元素类型.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误. 如果 stream 不是一个二进制输出流, 那么应该发出一个 error 类型的错误.

        如果 byte 不是流 stream 的一个流元素类型的整数, 可能会发出一个 type-error 类型的错误.

* 也见(See Also):

        read-byte, write-char, write-sequence

* 注意(Notes): None. 


### <span id="F-PEEK-CHAR">函数 PEEK-CHAR</span>

* 语法(Syntax):

        peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p => char

* 参数和值(Arguments and Values):

        peek-type---一个字符或 t 或 nil.
        input-stream---输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        char---一个字符或 eof-value.

* 描述(Description):
<!--TODO 待校验-->
        peek-char 在没有实际读取的情况下获取流 input-stream 中的下一个字符, 这样就可以在以后的时间里读取这个字符. 它还可以用于跳过并丢弃输入流 input-stream 中的中间字符, 直到找到一个特定的字符为止.

        如果 peek-type 没有被提供或者是 nil, peek-char 返回要从 input-stream 中读取的下一个字符, 实际上没有把它从 input-stream 中移除. 下一次从 input-stream 完成输入时, 这个字符仍然在那里. 如果 peek-type 是 t, 那么 peek-char 跳过空白字符, 但是不跳过注释, 然后在下一个字符执行窥视操作. 最后一个被检验的字符, 开始一个对象的那个, 不会从 input-stream 中被移除. 如果 peek-type 是一个字符, 那么 peek-char 跳过输入字符直到找到一个和那个字符 char= 的字符; 那个字符留在 input-stream 中.

        如果到达了文件的末尾并且 eof-error-p 是 false, 就返回 eof-value.

        如果 recursive-p 是 true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        当 input-stream 是一个回音流时, 只是被窥视的字符不会被重复. 在这个 peek-type 不是 nil 的情况中, 通过 peek-char 传递的这些字符会被当作通过 read-char 一样处理, 因此, 除非它们被 unread-char 标记出来否则就会得到回响.

* 示例(Examples):

    ```LISP
    (with-input-from-string (input-stream "    1 2 3 4 5")
        (format t "~S ~S ~S" 
                (peek-char t input-stream)
                (peek-char #\4 input-stream)
                (peek-char nil input-stream)))
    >>  #\1 #\4 #\4
    =>  NIL
    ```

* 受此影响(Affected By):

        *readtable*, *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        如果 eof-error-p 是 true 并且到达文件的末尾, 就会发出一个 end-of-file 类型的错误.

        如果 peek-type 是一个字符, 到达文件的末尾, 并且 eof-error-p 是 true, 就会发出一个 end-of-file 类型的错误.

        如果 recursive-p 是 true 到达文件的末尾, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-READ-CHAR">函数 READ-CHAR</span>

* 语法(Syntax):

        read-char &optional input-stream eof-error-p eof-value recursive-p => char

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        char---一个字符或 eof-value.

* 描述(Description):

        read-char 从 input-stream 返回下一个字符.

        当 input-stream 是一个回音流时, 在第一次看到这个字符时这个字符在 input-stream 上被响应. 不被read-char 所响应的字符是 unread-char 放在那里的, 因此被认为已经在之前的一次对 read-char 的调用中得到了响应.

        如果 recursive-p 是 true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        如果到达文件的末尾并且 eof-error-p 是 false, 就返回 eof-value.

* 示例(Examples):

    ```LISP
    (with-input-from-string (is "0123")
        (do ((c (read-char is) (read-char is nil 'the-end)))
            ((not (characterp c)))
        (format t "~S " c)))
    >>  #\0 #\1 #\2 #\3
    =>  NIL
    ```

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        如果在一个字符可以被读取之前到达文件的末尾, 并且 eof-error-p 是 true, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also):

        read-byte, read-sequence, write-char, read

* 注意(Notes):

        对应的输入函数是 write-char. 


### <span id="F-READ-CHAR-NO-HANG">函数 READ-CHAR-NO-HANG</span>

* 语法(Syntax):

        read-char-no-hang &optional input-stream eof-error-p eof-value recursive-p => char

* 参数和值(Arguments and Values):

        input-stream -- 一个输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        char---一个字符或 nil 或 eof-value.

* 描述(Description):

        如果一个字符可用, read-char-no-hang 就从 input-stream 返回一个字符. 如果没有字符可用, read-char-no-hang 就返回 nil.

        如果 recursive-p 是 true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        如果到达文件末尾并且 eof-error-p 是 false, 就返回 eof-value.

* 示例(Examples):

    ```LISP
    ;; This code assumes an implementation in which a newline is not
    ;; required to terminate input from the console.
    (defun test-it ()
      (unread-char (read-char))
      (list (read-char-no-hang) 
            (read-char-no-hang) 
            (read-char-no-hang)))
    =>  TEST-IT
    ;; Implementation A, where a Newline is not required to terminate
    ;; interactive input on the console.
    (test-it)
    >>  a
    =>  (#\a NIL NIL)
    ;; Implementation B, where a Newline is required to terminate
    ;; interactive input on the console, and where that Newline remains
    ;; on the input stream.
    (test-it)
    >>  a<NEWLINE>
    =>  (#\a #\Newline NIL)
    ```

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        当 eof-error-p 是 true 时, 如果到达文件末尾, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also):

        listen

* 注意(Notes):

        read-char-no-hang 准确地像 read-char, 除了如果有必要去等待来获取一个字符 (比如来自于键盘), 会在没有等待的情况下立即返回 nil. 


### <span id="F-TERPRI-FRESH-LINE">函数 TERPRI, FRESH-LINE</span>

* 语法(Syntax):

        terpri &optional output-stream => nil

        fresh-line &optional output-stream => generalized-boolean

* 参数和值(Arguments and Values):

        output-stream -- 一个输出流标识符. 默认是标准输出.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        terpri 输出一个新行到 output-stream 中.

        fresh-line 类似于 terpri 但是当且仅当这个 output-stream 没有在一行的开头时输出一个新行. 如果出于某个原因不能确定这个, 那么无论怎样都会输出一个新行. 如果 fresh-line 输出一个新行就返回 true; 否则它返回 false.

* 示例(Examples):

    ```LISP
    (with-output-to-string (s)
        (write-string "some text" s)
        (terpri s)
        (terpri s)
        (write-string "more text" s))
    =>  "some text

    more text"
    (with-output-to-string (s)
        (write-string "some text" s)
        (fresh-line s)
        (fresh-line s)
        (write-string "more text" s))
    =>  "some text
    more text"
    ```

* 副作用(Side Effects):

        这个 output-stream 会被修改.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*.

* 异常情况(Exceptional Situations):

        None.

* 也见(See Also): None.

* 注意(Notes):

      terpri 和下面这个效果是一样的

      (write-char #\Newline output-stream)


### <span id="F-UNREAD-CHAR">函数 UNREAD-CHAR</span>

* 语法(Syntax):

        unread-char character &optional input-stream => nil

* 参数和值(Arguments and Values):

        character---一个字符; 必须是从 input-stream 读到的最后一个字符.
        input-stream---一个输入流标识符. 默认是标准输入.

* 描述(Description):

        unread-char 把 character 放置回 input-stream 的前面, 这样一来它会再一次成为 input-stream 中的下一个字符.

        当 input-stream 是一个回音流时, 不要尝试去撤销字符 character 已经在 input-stream 上完成的任何响应. 然而, 通过 unread-char 放置在 input-stream 的字符以这样方式被标记来抑制后来通过 read-char 的再响应.

        在中间没有在一个流上调用 read-char (或者某个其他隐式读取字符的输入操作) 的情况下, 在同一个流上连续两次调用 unread-char 是一个错误.

        调用 peek-char 或 read-char 提交所有前面的字符. 在 peek-char 返回的任何字符之前调用 unread-char (包括由 peek-char 传递的那些具有非 nil peek-type 类型的字符) 的后果是不明确的. 特别是, 在 peek-char 之后调用 unread-char 的后果是未指定的.

* 示例(Examples):

    ```LISP
    (with-input-from-string (is "0123")
        (dotimes (i 6)
          (let ((c (read-char is)))
            (if (evenp i) (format t "~&~S ~S~%" i c) (unread-char c is)))))
    >>  0 #\0
    >>  2 #\1
    >>  4 #\2
    =>  NIL
    ```LISP

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        peek-char, read-char, 章节 21.1 (流的概念)

* 注意(Notes):

        unread-char 旨在成为一种有效的机制, 允许 Lisp 读取器和其他解析器在输入流 input-stream 中执行一个字符的展望. 


### <span id="F-WRITE-CHAR">函数 WRITE-CHAR</span>

* 语法(Syntax):

        write-char character &optional output-stream => character

* 参数和值(Arguments and Values):

        character---一个字符.
        output-stream -- 一个输出流标识符. 默认是标准输出.

* 描述(Description):

        write-char 把字符 character 输出到 output-stream.

* 示例(Examples):

    ```LISP
    (write-char #\a)
    >>  a
    =>  #\a
    (with-output-to-string (s) 
      (write-char #\a s)
      (write-char #\Space s)
      (write-char #\b s))
    =>  "a b"
    ```

* 副作用(Side Effects):

        流 output-stream 会被修改.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        read-char, write-byte, write-sequence

* 注意(Notes): None. 


### <span id="F-READ-LINE">函数 READ-LINE</span>

* 语法(Syntax):

        read-line &optional input-stream eof-error-p eof-value recursive-p
        => line, missing-newline-p

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        line---一个字符串或 eof-value.
        missing-newline-p---一个广义 boolean.

* 描述(Description):

        从 input-stream 读取一个文本行, 由一个新行标识或文件末尾来结尾.

        If recursive-p is true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        这个主要的值, line, 是读取到的那行, 表示为一个字符串 (如果有这个字符串的话, 不带末尾的新行标识). 如果 eof-error-p 是 false 并且在读到任何字符之前到达 input-stream 的文件末尾, 返回 eof-value 作为这个 line.

        第二个值, missing-newline-p, 是一个广义 boolean, 如果 line 是由新行标识来终止的就是 false, 如果 line 由 input-stream 文件结尾来终止(或者如果这个 line 就是 eof-value)就是 true .

* 示例(Examples):

    ```LISP
    (setq a "line 1
    line2")
    =>  "line 1
    line2"
    (read-line (setq input-stream (make-string-input-stream a)))
    =>  "line 1", false
    (read-line input-stream)
    =>  "line2", true
    (read-line input-stream nil nil)
    =>  NIL, true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        如果在这行中任何字符被读取到之前就到达文件的末尾, 如果 eof-error-p 是 true 就会发出一个错误.

* 也见(See Also):

        read

* 注意(Notes):

        对应输出函数是 write-line.


### <span id="F-WRITE-STRING-WRITE-LINE">函数 WRITE-STRING, WRITE-LINE</span>

* 语法(Syntax):

        write-string string &optional output-stream &key start end => string

        write-line string &optional output-stream &key start end => string

* 参数和值(Arguments and Values):

        string---一个字符串.
        output-stream -- 一个输出流标识符. 默认是标准输出.
        start, end---字符串 string 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.

* 描述(Description):

        write-string 把字符串 string 由 start 和 end 限定的子字符串的字符写到 output-stream 中. write-line 做相同的事, 但是会在之后输出一个新行标识.

* 示例(Examples):

    ```LISP
    (prog1 (write-string "books" nil :end 4) (write-string "worms"))
    >>  bookworms
    =>  "books"
    (progn (write-char #\*)
            (write-line "test12" *standard-output* :end 5) 
            (write-line "*test2")
            (write-char #\*)
            nil)
    >>  *test1
    >>  *test2
    >>  *
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        read-line, write-char

* 注意(Notes):

        write-line 和 write-string 返回 string, 不是由 start 和 end 限定的子字符串.

        (write-string string)
        ==  (dotimes (i (length string)
              (write-char (char string i)))

        (write-line string)
        ==  (prog1 (write-string string) (terpri))

### <span id="F-READ-SEQUENCE">函数 READ-SEQUENCE</span>

* 语法(Syntax):

        read-sequence sequence stream &key start end => position

* 参数和值(Arguments and Values):

        sequence---一个序列.
        stream---一个输入流.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        position---一个大于等于零, 并且小于等于序列 sequence 长度的整数.

* 描述(Description):

        通过用读取自流 stream 的元素来替换序列 sequence 中由 start 和 end 限定的元素来破坏性地修改序列 sequence.

        通过从流 stream 中拷贝连续元素到序列 sequence 来破坏性地修改它. 如果在拷贝这个子序列的所有元素之前到达这个流 stream 的文件末尾, 那么在序列 sequence 的末尾旁边的元素不会被更新.

        position 是序列 sequence 中第一个没有被更新的元素的索引, 由于到达文件的末尾的话它可能小于 end.

* 示例(Examples):

    ```LISP
    (defvar *data* (make-array 15 :initial-element nil))
    (values (read-sequence *data* (make-string-input-stream "test string")) *data*)
    =>  11, #(#\t #\e #\s #\t #\Space #\s #\t #\r #\i #\n #\g NIL NIL NIL NIL)
    ```

* 副作用(Side Effects):

        修改流 stream 和 sequence.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 就应该准备发出一个 type-error 类型的错误. 如果 start 不是一个非负整数, 就会发出一个 type-error 类型的错误. 如果 end 不是一个非负整数或 nil, 就会发出一个 type-error 类型的错误.

        如果读取自流 stream 的一个元素不是 sequence 的一个元素类型的成员, 就会发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.2.1 (Compiler Terminology), write-sequence, read-line

* 注意(Notes):

        read-sequence 和遍历指定的子序列并每次从流 stream 中读取一个元素并将其存储到序列 sequence 中的效果是相同的, 但是可能比等价循环更有效率. 一个有效的实现更有可能存在于 sequence 是一个具有与 stream 相同的元素类型的向量的情况下. 


### <span id="F-WRITE-SEQUENCE">函数 WRITE-SEQUENCE</span>

* 语法(Syntax):

        write-sequence sequence stream &key start end => sequence

* 参数和值(Arguments and Values):

        sequence---一个序列.
        stream---一个输出流.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.

* 描述(Description):

        write-sequence 把序列 sequence 中由 start 和 end 限定的子序列写入到流 stream 中.

* 示例(Examples):

    ```LISP
    (write-sequence "bookworms" *standard-output* :end 4)
    >>  book
    =>  "bookworms"
    ```

* 副作用(Side Effects):

        修改流 stream.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 就应该准备发出一个 type-error 类型的错误. 如果 start 不是一个非负整数, 就会发出一个 type-error 类型的错误. 如果 end 不是一个非负整数或 nil, 就会发出一个 type-error 类型的错误.

        如果限定的序列 sequence 的一个元素不是流 stream 的流元素类型的一个成员, 可能会发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.2.1 (Compiler Terminology), read-sequence, write-string, write-line

* 注意(Notes):

        write-sequence 和遍历指定的子序列并每次写入一个元素到流 stream 中的效果是相同的, 但是可能比等价循环更有效率. 一个有效的实现更有可能存在于 sequence 是一个具有与 stream 相同的元素类型的向量的情况下.


### <span id="F-FILE-LENGTH">函数 FILE-LENGTH</span>

* 语法(Syntax):

        file-length stream => length

* 参数和值(Arguments and Values):

        stream---和一个文件关联的流.
        length---一个非负整数或 nil.

* 描述(Description):

        file-length 返回流 stream 的长度, 如果长度不能确定就返回 nil.

        对于一个二进制文件, 长度是以流 stream 的元素类型的单位来测量的.

* 示例(Examples):

    ```LISP
    (with-open-file (s "decimal-digits.text" 
                        :direction :output :if-exists :error)
      (princ "0123456789" s)
      (truename s))
    =>  #P"A:>Joe>decimal-digits.text.1"
    (with-open-file (s "decimal-digits.text")
      (file-length s))
    =>  10
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果流 stream 不是一个和文件关联的流就会发出一个 type-error 类型的错误.

* 也见(See Also):

        open

* 注意(Notes): None. 

### <span id="F-FILE-POSITION">函数 FILE-POSITION</span>

* 语法(Syntax):

        file-position stream => position

        file-position stream position-spec => success-p

* 参数和值(Arguments and Values):

        stream---一个流.
        position-spec---一个文件位置标识符.
        position---一个文件位置或 nil.
        success-p---一个广义 boolean.

* 描述(Description):

        返回或改变一个流 stream 中的当前位置.

        当没有提供 position-spec 时, file-position 在流 stream 中的当前文件位置, 如果不能确定就返回 nil.

        当提供了 position-spec 时, 在流 stream 中的文件位置会被设置为那个文件位置 (如果可能的话). 如果这个重定位执行成功, file-position 返回 true, 如果没有就返回 false.

        由但参数的 file-position 返回的整数用作同一个文件的 position-spec 应该是可接受的.

        对于一个字符文件, 执行一个单独的 read-char 或 write-char 操作可能造成这个文件位置递增超过 1, 因为字符集转换 because of character-set translations (比如在 Common Lisp f#\Newline 字符和一外部的 ASCII 回车/换行序列之间的转换) 以及具体实现的其他方面. 对于一个二进制文件, 每个 read-byte 或 write-byte 操作递增文件位置 1.

* 示例(Examples):

    ```LISP
    (defun tester ()
      (let ((noticed '()) file-written)
        (flet ((notice (x) (push x noticed) x))
          (with-open-file (s "test.bin" 
                              :element-type '(unsigned-byte 8)
                              :direction :output
                              :if-exists :error)
              (notice (file-position s)) ;1
              (write-byte 5 s) 
              (write-byte 6 s)
              (let ((p (file-position s)))
                (notice p) ;2
                (notice (when p (file-position s (1- p))))) ;3
              (write-byte 7 s)
              (notice (file-position s)) ;4
              (setq file-written (truename s)))
            (with-open-file (s file-written
                              :element-type '(unsigned-byte 8)
                              :direction :input)
              (notice (file-position s)) ;5
              (let ((length (file-length s)))
                (notice length) ;6
                (when length
                  (dotimes (i length)
                    (notice (read-byte s)))))) ;7,...
            (nreverse noticed))))
    =>  tester
    (tester)
    =>  (0 2 T 2 0 2 5 7)
    OR=>  (0 2 NIL 3 0 3 5 6 7)
    OR=>  (NIL NIL NIL NIL NIL NIL)
    ```

* 副作用(Side Effects):

        当提供了这个 position-spec 参数时, 在这个 stream 中的文件位置可能被移动.

* 受此影响(Affected By):

        由 file-position 返回的值随着输入或输出操作的执行单调递增.

* 异常情况(Exceptional Situations):

        如果提供了 position-spec, 但是太大或者不合适, 就会发出一个错误.

* 也见(See Also):

        file-length, file-string-length, open

* 注意(Notes):

        具有被表示为一个有界大小的记录序列的字符文件的具体实现可能选择去对这个文件位置进行编码, 比如, <<record-number>>*<<max-record-size>>+<<character-within-record>>. 这是一个有效的编码因为它随着每个字符被读取或写入单调递增, 尽管每次步进没有必要是 1. 作为给 file-position 的 position-spec, 如果在解码成记录的数字和字符号时, 结果表明所提供的记录对于指定的字符数来说太短了, 一个整数可能被认为是 "不合适的". <!--TODO 待校验-->


### <span id="F-FILE-STRING-LENGTH">函数 FILE-STRING-LENGTH</span>

* 语法(Syntax):

        file-string-length stream object => length

* 参数和值(Arguments and Values):

        stream---一个输出字符文件流.
        object---一个字符串或一个字符.
        length---一个非负整数, 或 nil.

* 描述(Description):

        file-string-length 返回 (file-position stream) 的当前值和它在写入对象 object 之后的值的区别, 如果不能确定就是 nil.

        返回值对应这个调用时流 stream 的当前状态, 当这个流 stream 的状态改变时再一次调用的值可能是不同的.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-OPEN">函数 OPEN</span>

* 语法(Syntax):

        open filespec &key direction element-type if-exists if-does-not-exist external-format
        => stream

* 参数和值(Arguments and Values):

        filespec---一个路径名标识符.
        direction---:input, :output, :io, 或 :probe 其中之一. 默认是 :input.
        element-type---一个 character 的可识别子类型的类型指定负; 或者一个 integer 的有限可识别子类型; 或者符号 signed-byte, unsigned-byte, 或 :default 的其中一个. 默认是 character.
        if-exists---:error, :new-version, :rename, :rename-and-delete, :overwrite, :append, :supersede, 或 nil 其中之一. 如果 filespec 的版本成员是 :newest, 默认就是 :new-version, 否则就是 :error.
        if-does-not-exist---:error, :create, 或 nil 其中之一. 如果 direction 是 :input 或者 if-exists 是 :overwrite 或 :append, 默认就是 :error; 如果 direction 是 :output 或 :io, 并且 if-exists 既不是 :overwrite 也不是 :append, 那么就是 :create; 当 direction 是 :probe 时就是 nil.
        external-format---一个额外的文件格式化标识符. 默认是 :default.
        stream---一个文件流或 nil.

* 描述(Description):

        open 创建, 打开, 并返回一个连接到 filespec 指定文件的文件流. filespec 是这个要被打开的文件的名字. 如果这个 filespec 标识符是一个流, 那个流首先没有被关闭否则就会被影响.

        给 open 的关键字参数指定了返回的那个文件流的特质, 以及如何处理错误.

        如果 direction 是 :input 或 :probe, 或者如果 if-exists 不是 :new-version 而这个 filespec 的版本成员是 :newest, 那么这个打开的文件就是已经存在于那个文件系统中的文件, 该文件的版本比文件系统中的任何其他文件都要大, 其其他路径名成员与 filespec 相同.

        一个具体实现需要去识别所有这些 open 关键字选项并且在这个主机操作系统的上下文中去做一些合理的事. 比如, 如果一个文件系统不支持不同的文件版本, 并且不区分删除(deletion)和除去(expunging)的概念, :new-version 可能被当作和 :rename 或 :supersede 一样, 而 :rename-and-delete 可能被当作和 :supersede 一样.

        :direction

            这些是 direction 的可能的值, 以及它们如何影响创建的流的性质:

            :input

                导致创建一个输入文件流.

            :output

                导致创建一个输出文件流.

            :io

                导致创建一个双向文件流.

            :probe

                导致创建一个 "无方向(no-directional)" 文件流; 实际上, 这个文件流是在 open 返回之前创建并关闭的.

        :element-type

            这个 element-type 指定文件流的事务单元. 如果它是 :default, 这个单元由文件系统决定, 可能基于这个文件.

        :if-exists

            if-exists 指定了如果 direction 是 :output 或 :io 而名为 filespec 的文件已经存在的话要采取的动作. 如果 direction 是 :input, 没有提供, 或 :probe, if-exists 会被忽略. 这些是 open 被 if-exists 修改的结果:

            :error

                发出一个 file-error 类型的错误.

            :new-version

                用一个更大的版本数字创建一个新文件.

            :rename

                已存在的文件会重命名为某个其他的名字并且创建一个新文件.

            :rename-and-delete

                已存在的文件会重命名为某个其他的名字, 它会被删除(delete)但不会被除去(expunge), 然后创建一个新文件.

            :overwrite

                在这个流上的输出操作会破坏性地修改这个已存在的文件. 如果 direction 是 :io 那么这个文件会以同时允许读写的双向模式被打开. 这个文件指针的初始定位于这个文件的开始; 然而, 当文件打开时, 文件不会被截断为零.

            :append

                在这个流上的输出操作会破坏性地修改这个已存在的文件. 这个文件指针的初始定位于这个文件的末尾.

                如果 direction 是 :io, 那么这个文件会以同时允许读写的双向模式被打开.

            :supersede

                已存在的文件会被取代; 这就是说, 一个和旧的那个有着相同名字的新文件会被创建. 如果可能的话, 具体实现直到这个新的流关闭之前都不应该毁掉这个旧文件.

            nil

                没有文件或流会被创建; 反而, 返回 nil 来表示这个失败.

        :if-does-not-exist

            if-does-not-exist 指定了名为 filespec 的文件不存在的话要采取的动作. 这些是 open 被 if-does-not-exist 修改的结果:

            :error

                发出一个 file-error 类型的错误.

            :create

                创建一个空文件. 处理过程继续, 好像文件已经存在, 但是没有执行 if-exists 所指示的处理.

            nil

                没有文件或流会被创建; 反而, 返回 nil 来表示这个失败.

        :external-format

            这个选项为文件选择一个外部文件格式: 这个选项的仅有标准化值是 :default, 尽管具体实现允许去定义额外的文件格式并且 stream-external-format 返回的依赖于具体实现的值也可以被符合规范的程序所使用.

            这个 external-format 对于所有元素类型为 character 的一个子类的任何种类的文件流都是有意义的. 对于对这个选项无意义的流会忽略这个选项; 然而, 具体实现可能定义其他有意义的元素类型. 如果写入一个不能被给定外部文件格式所表示的字符, 后果是未指定的.

        当一个文件被打开时, 文件流被构造成文件系统到 Lisp 环境的代表; 文件流上的操作反映在文件系统中的文件操作上.

        一个文件可以被 open 删除, 重命名, 或破坏性修改.

        关于打开相对路径名的信息, 见章节 19.2.3 (Merging Pathnames).

* 示例(Examples):

    ```LISP
    (open filespec :direction :probe)  =>  #<Closed Probe File Stream...>
    (setq q (merge-pathnames (user-homedir-pathname) "test"))
    =>  #<PATHNAME :HOST NIL :DEVICE device-name :DIRECTORY directory-name
        :NAME "test" :TYPE NIL :VERSION :NEWEST>
    (open filespec :if-does-not-exist :create) =>  #<Input File Stream...>
    (setq s (open filespec :direction :probe)) =>  #<Closed Probe File Stream...>
    (truename s) =>  #<PATHNAME :HOST NIL :DEVICE device-name :DIRECTORY
        directory-name :NAME filespec :TYPE extension :VERSION 1>
    (open s :direction :output :if-exists nil) =>  NIL 
    ```

* 受此影响(Affected By):

        主机计算机文件系统的性质和状态.

* 异常情况(Exceptional Situations):

        如果 if-exists 是 :error, (受限于上面列出的 if-exists 含义的限制), 就会发出一个 file-error 类型的错误.

        如果 if-does-not-exist 是 :error (受限于上面列出的 if-does-not-exist 含义的限制), 就会发出一个 file-error 类型的错误.

        如果一个实现不可能以接近这里指定的方式处理某些选项, 就会发出一个 error 类型的错误.

        如果 (wild-pathname-p filespec) 返回 true 就会发出一个 file-error 类型的错误.

        如果 external-format 不被具体实现所接受, 就会发出一个 error 类型的错误.

        目前存在的各种文件系统具有广泛的不同功能, 并且文件系统的某些方面超出了该规范定义的范围. 一个给定的实现可能无法以完全相同的方式支持所有这些选项. 一个实现需要去识别所有这些选项关键字, 并尝试在主机文件系统的上下文中执行一些"合理"的操作. 在必要的情况下, 为了适应文件系统, 一个实现稍微偏离了这里指定的语义, 而不被取消作为一个符合规范的实现的资格. 如果实现以类似于此处指定的方式处理某些选项是完全不可能的, 那么它可能只是发出一个错误.

        关于 :element-type 选项, 如果一个请求的类型不被文件系统所支持, 在升级中进行的类型替换是允许的 a substitution of types such as that which goes on in upgrading is permissible. 作为一个最小的需求, 应该是这样的情况: 在给定的元素类型中打开一个输出流, 然后在相同的元素类型中打开同一个文件的输入流应该是兼容的.

* 也见(See Also):

        with-open-file, close, pathname, logical-pathname, 章节 19.2.3 (Merging Pathnames), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

        当一个反常的退出发生时, open 不会自动关闭这个文件.

        当 element-type 是 character 的一个子类型时, read-char 和/或 write-char 可以在产生的文件流上被使用.

        当 element-type 是 integer 的一个子类型时, read-byte 和/或 write-byte 可以在产生的文件流上被使用.

        当 element-type 是 :default 时, 类型可以通过使用 stream-element-type 来确定. 


### <span id="F-STREAM-EXTERNAL-FORMAT">函数 STREAM-EXTERNAL-FORMAT</span>

* 语法(Syntax):

        stream-external-format stream => format

* 参数和值(Arguments and Values):

        stream---一个文件流.
        format---一个外部文件格式.

* 描述(Description):

        返回流 stream 的一个外部文件格式标识符.

* 示例(Examples):

    ```LISP
    (with-open-file (stream "test" :direction :output)
      (stream-external-format stream))
    =>  :DEFAULT
    OR=>  :ISO8859/1-1987
    OR=>  (:ASCII :SAIL)
    OR=>  ACME::PROPRIETARY-FILE-FORMAT-17
    OR=>  #<FILE-FORMAT :ISO646-1983 2343673>
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        给函数 open 和宏 with-open-file 的 :external-format 参数.

* 注意(Notes):

        这个返回的格式没有必要对于其他实现是有意义的. 


### <span id="M-WITH-OPEN-FILE">宏 WITH-OPEN-FILE</span>

* 语法(Syntax):

        with-open-file (stream filespec options*) declaration* form*
        => results

* 参数和值(Arguments and Values):

        stream -- 一个变量.
        filespec---一个路径名标识符.
        options -- 表达式形式; 求值的.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式 progn.
        results---由表达式形式 forms 返回的值.

* 描述(Description):

        with-open-file 使用 open 来创建一个到名为 filespec 的文件的文件流. filespec 是要被打开的文件的名字. options 是用作给 open 的关键字参数的那些.

        这个 stream 变量绑定的流对象有着动态范围; 它的范围在表达式形式退出时结束.

        with-open-file 像一个隐式的 progn 求值表达式形式 forms, 其中流 stream 被绑定到 open 返回的值上.

        当控制离开主体时, 不管是正常的还是反常的 (比如通过使用 throw), 这个文件会被自动关闭. 如果一个新的输出文件要被写入, 而控制不正常地离开了, 这个文件会被终止并且文件系统被保留, 尽可能地保留, 就好像文件从来没有打开过一样.

        对于流 stream 使用 :if-exists nil 或 :if-does-not-exist nil 来绑定为 nil 是可能的. :if-does-not-exist nil 的使用者应该检查是否为一个有效的流.

        如果尝试去对流变量赋值, 后果是未定义的. 如果检测到这样的尝试, 编译器可能选择去发出一个警告.

* 示例(Examples):

    ```LISP
    (setq p (merge-pathnames "test"))
    =>  #<PATHNAME :HOST NIL :DEVICE device-name :DIRECTORY directory-name
        :NAME "test" :TYPE NIL :VERSION :NEWEST>
    (with-open-file (s p :direction :output :if-exists :supersede)
        (format s "Here are a couple~%of test data lines~%")) =>  NIL
    (with-open-file (s p)
        (do ((l (read-line s) (read-line s nil 'eof)))
            ((eq l 'eof) "Reached end of file.")
        (format t "~&*** ~A~%" l)))
    >>  *** Here are a couple
    >>  *** of test data lines
    =>  "Reached end of file."

    ;; Normally one would not do this intentionally because it is
    ;; not perspicuous, but beware when using :IF-DOES-NOT-EXIST NIL
    ;; that this doesn't happen to you accidentally...
    (with-open-file (foo "no-such-file" :if-does-not-exist nil)
      (read foo))
    >>  hello?
    =>  HELLO? ;This value was read from the terminal, not a file!

    ;; Here's another bug to avoid...
    (with-open-file (foo "no-such-file" :direction :output :if-does-not-exist nil)
      (format foo "Hello"))
    =>  "Hello" ;FORMAT got an argument of NIL!
    ```

* 副作用(Side Effects):

        创建一个名为 filename 的文件的流 (进入时), 并且关闭这个流 (退出时). 在某些实现中, 在这个文件被打开时它可能被锁定. 如果这个流是一个输出流, 可能会创建一个文件.

* 受此影响(Affected By):

        主机计算机文件系统.

* 异常情况(Exceptional Situations):

        见函数 open.

* 也见(See Also):

        open, close, pathname, logical-pathname, 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-CLOSE">函数 CLOSE</span>

* 语法(Syntax):

        close stream &key abort => result

* 参数和值(Arguments and Values):

        stream---一个流 (不管是打开的还是关闭的).
        abort---一个广义 boolean. 默认是 false.
        result---如果这个流 stream 在它被接受作为参数时是打开的就是 t, 否则就是依赖于具体实现的.

* 描述(Description):

        close 关闭流 stream. 关闭一个流意味着它可能不再被用于输入或输出操作了. 关闭一个文件流的行为会结束这个流 stream 和它关联的文件之间的关联; 与文件系统的事务会结束, 而输入/输出可能不再在这个流 stream 上执行.

        如果 abort 是 true, 会尝试去清理已经创建的流的任何副作用. 如果流 stream 执行输出到一个在这个流 stream 被创建时被创建的文件, 这个文件会被删除并且任何之前存在的文件不会被取代.

        允许去关闭一个已经关闭的流, 但是在这个情况中结果是依赖于具体实现的.

        在流 stream 被关闭后, 仍然可能在它上面执行一下查询操作: streamp, pathname, truename, merge-pathnames, pathname-host, pathname-device, pathname-directory,pathname-name, pathname-type, pathname-version, namestring, file-namestring, directory-namestring, host-namestring, enough-namestring, open, probe-file, 和 directory.

        在一个构造的流上的 close 的效果只是去关闭这个参数 stream. 对复合流的组成部分没有影响.

        对于一个用 make-string-output-stream 创建的流, 在 close 后的 get-output-stream-string 结果是未指定的.

* 示例(Examples):

    ```LISP
    (setq s (make-broadcast-stream)) =>  #<BROADCAST-STREAM>
    (close s) =>  T
    (output-stream-p s) =>  true
    ```

* 副作用(Side Effects):

        流 stream 会被关闭 (如果必要的话). 如果 abort 是 true 并且这个流 stream 是一个输出文件流, 它关联的文件可能被删除.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        open

* 注意(Notes): None. 


### <span id="M-WITH-OPEN-STREAM">宏 WITH-OPEN-STREAM</span>

* 语法(Syntax):

        with-open-stream (var stream) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        var---一个变量名字.
        stream---一个表达式形式; 求值来产生一个流.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式 progn.
        results---由这些表达式形式 forms 返回的结果.

* 描述(Description):

        with-open-stream 在流 stream 上执行一系列操作, 并返回一个值, 然后关闭这个流 stream.

        var 被绑定为这个流 stream 的值, 然后这些表达式形式 forms 就像一个隐式的 progn 一样被求值. 流 stream 在从 with-open-stream 退出时会被自动关闭, 不管这个退出是正常的还是不正常的. 流 stream 有着动态范围; 它的范围在这个表达式形式退出时结束.

        如果在这些表达式形式 forms 中尝试去对变量 var 赋值后果是未定义的.

* 示例(Examples):

    ```LISP
    (with-open-stream (s (make-string-input-stream "1 2 3 4"))
        (+ (read s) (read s) (read s))) =>  6
    ```

* 副作用(Side Effects):

        这个流 stream 会被关闭 (退出时).

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        close

* 注意(Notes): None. 

### <span id="F-LISTEN">函数 LISTEN</span>

* 语法(Syntax):

        listen &optional input-stream => generalized-boolean

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符. 默认是标准输入.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果从输入流 input-stream 立即有一个字符可用就返回 true; 赋值, 返回 false. 在一个非交互式输入流上, listen 返回 true 除了到文件末尾时. 如果到达文件的末尾, listen 返回 false. 当输入流 input-stream 从一个交互式设备如键盘中获得字符时, 就会使用 listen.

* 示例(Examples):

    ```LISP
    (progn (unread-char (read-char)) (list (listen) (read-char)))
    >>  1
    =>  (T #\1)
    (progn (clear-input) (listen))
    =>  NIL ;Unless you're a very fast typist!
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-input*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        interactive-stream-p, read-char-no-hang

* 注意(Notes): None. 


### <span id="F-CLEAR-INPUT">函数 CLEAR-INPUT</span>

* 语法(Syntax):

        clear-input &optional input-stream => nil

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符. 默认是标准输入.

* 描述(Description):

        从输入流 input-stream 中清理任何可用的输入.

        如果 clear-input 对输入流 input-stream 没有意义, 那么 clear-input 什么都不做.

* 示例(Examples):

    ```LISP
    ;; The exact I/O behavior of this example might vary from implementation
    ;; to implementation depending on the kind of interactive buffering that
    ;; occurs.  (The call to SLEEP here is intended to help even out the 
    ;; differences in implementations which do not do line-at-a-time buffering.)

    (defun read-sleepily (&optional (clear-p nil) (zzz 0))
      (list (progn (print '>) (read))
            ;; Note that input typed within the first ZZZ seconds 
            ;; will be discarded.
            (progn (print '>) 
                  (if zzz (sleep zzz))
                  (print '>>)
                  (if clear-p (clear-input))
                  (read))))

    (read-sleepily)
    >>  > 10
    >>  >
    >>  >> 20
    =>  (10 20)

    (read-sleepily t)
    >>  > 10
    >>  >
    >>  >> 20
    =>  (10 20)

    (read-sleepily t 10)
    >>  > 10
    >>  > 20  ; Some implementations won't echo typeahead here.
    >>  >> 30
    =>  (10 30)
    ```

* 副作用(Side Effects):

        输入流 input-stream 会被修改.

* 受此影响(Affected By):

        *standard-input*

* 异常情况(Exceptional Situations):

        如果 input-stream 不是一个流标识符, 就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        clear-output

* 注意(Notes): None. 

### <span id="F-FINISH-AND-FORCE-AND-CLEAR-OUTPUT">函数 FINISH-OUTPUT, FORCE-OUTPUT, CLEAR-OUTPUT</span>

* 语法(Syntax):

        finish-output &optional output-stream => nil

        force-output &optional output-stream => nil

        clear-output &optional output-stream => nil

* 参数和值(Arguments and Values):

        output-stream---一个输入流标识符. 默认是标准输出.

* 描述(Description):

        finish-output, force-output, 和 clear-output 可以控制缓冲流输出的内部处理.

        finish-output 尝试去确保任何发送到 output-stream 的缓冲输出已经到达目的地, 然后返回.

        force-output 启动了任何一个内部缓冲区的清空, 但不等待完成或确认返回.

        clear-output 试图中止任何正在进行中的未完成的输出操作, 以便尽可能少的输出以继续到达目的地.

        如果这些操作的任何一个对于 output-stream 没有意义, 那么它什么都不做. 这些函数的准确行为是依赖于具体实现的.

* 示例(Examples):

    ```LISP
    ;; Implementation A
    (progn (princ "am i seen?") (clear-output))
    =>  NIL

    ;; Implementation B
    (progn (princ "am i seen?") (clear-output))
    >>  am i seen?
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-output*

* 异常情况(Exceptional Situations):

        如果 output-stream 不是一个流标识符, 就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        clear-input

* 注意(Notes): None. 


### <span id="F-Y-OR-N-P-YES-OR-NO-P">函数 Y-OR-N-P, YES-OR-NO-P</span>

* 语法(Syntax):

        y-or-n-p &optional control &rest arguments => generalized-boolean

        yes-or-no-p &optional control &rest arguments => generalized-boolean

* 参数和值(Arguments and Values):

        control---一个格式控制.
        arguments---control 的格式参数.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        这些函数询问一个问题并且解析一个来自用户的响应. 如果回答是肯定的, 它们返回 true, 如果是否定的就是 false.

        y-or-n-p 被用于询问用户一个答案为 "yes" 或 "no" 的问题. 它的目的是要求用户用一个字符来回答一个 yes-or-no 的问题. yes-or-no-p 也被用于询问用户一个答案为 "yes" 或 "no" 的问题. 它的目的是要求用户采取更多的操作, 而不仅仅是一次击键, 比如输入完整的单词 "yes" 或 "no", 然后是换行符.

        y-or-n-p 输出一个信息 (如果提供的话), 以某种依赖于具体实现的方式读取一个回答 (目的是为了简短和简单, 比如读取一个字符, 比如 Y 或 N). yes-or-no-p 输出一个信息 (如果提供的话), 吸引用户的注意力 (比如, 通过终端的响铃), 并且以某种依赖于具体实现的方式读取一个回答 (目的是多字符, 比如 YES 或 NO).

        如果提供了 format-control 并且不是 nil, 那么会执行一个 fresh-line 操作; 然后打印一个信息, 就好像 format-control 和 arguments 给到 format 一样. 在任何情况下, 如果合适的话, yes-or-no-p 和 y-or-n-p 会提供一个提示, 比如 "(Y or N)" 或 "(Yes or No)".

        所有输入和输出执行都使用查询 I/O.

* 示例(Examples):

    ```LISP
    (y-or-n-p "(t or nil) given by")
    >>  (t or nil) given by (Y or N) Y
    =>  true
    (yes-or-no-p "a ~S message" 'frightening) 
    >>  a FRIGHTENING message (Yes or No) no
    =>  false
    (y-or-n-p "Produce listing file?") 
    >>  Produce listing file?
    >>  Please respond with Y or N. n
    =>  false
    ```

* 副作用(Side Effects):

        会发生查询 I/O 的输入或输出.

* 受此影响(Affected By):

        *query-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        format

* 注意(Notes):

        yes-or-no-p 和 yes-or-no-p 不会在提示字符串的末尾添加问号, 因此任何想要的问号或其他标点符号都应该显式地包含在文本查询中. 


### <span id="F-MAKE-SYNONYM-STREAM">函数 MAKE-SYNONYM-STREAM</span>

* 语法(Syntax):

        make-synonym-stream symbol => synonym-stream

* 参数和值(Arguments and Values):

        symbol---命名动态变量的一个符号.
        synonym-stream---一个 synonym-stream.

* 描述(Description):

        返回一个 synonym-stream 符号是 symbol 的 synonym-stream.

* 示例(Examples):

    ```LISP
    (setq a-stream (make-string-input-stream "a-stream")
            b-stream (make-string-input-stream "b-stream"))
    =>  #<String Input Stream> 
    (setq s-stream (make-synonym-stream 'c-stream))
    =>  #<SYNONYM-STREAM for C-STREAM> 
    (setq c-stream a-stream)
    =>  #<String Input Stream> 
    (read s-stream) =>  A-STREAM
    (setq c-stream b-stream)
    =>  #<String Input Stream> 
    (read s-stream) =>  B-STREAM
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的符号不是一个符号就应该发出 type-error.

* 也见(See Also):

        章节 21.1 (流的概念)

* 注意(Notes): None. 

### <span id="F-SYNONYM-STREAM-SYMBOL">函数 SYNONYM-STREAM-SYMBOL</span>

* 语法(Syntax):

        synonym-stream-symbol synonym-stream => symbol

* 参数和值(Arguments and Values):

        synonym-stream---一个 synonym-stream.
        symbol---一个符号.

* 描述(Description):

        返回这个 synonym-stream 正在使用的 symbol-value 的符号.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        make-synonym-stream

* 注意(Notes): None. 

### <span id="F-BROADCAST-STREAM-STREAMS">函数 BROADCAST-STREAM-STREAMS</span>

* 语法(Syntax):

        broadcast-stream-streams broadcast-stream => streams

* 参数和值(Arguments and Values):

        broadcast-stream---一个广播流.
        streams---一个流猎豹.

* 描述(Description):

        返回一个由这个广播流 broadcast-stream 正在广播的所有流组成的输出流列表.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MAKE-BROADCAST-STREAM">函数 MAKE-BROADCAST-STREAM</span>

* 语法(Syntax):

        make-broadcast-stream &rest streams => broadcast-stream

* 参数和值(Arguments and Values):

        stream---一个输出流.
        broadcast-stream---一个广播流.

* 描述(Description):

        返回一个广播流.

* 示例(Examples):

    ```LISP
    (setq a-stream (make-string-output-stream)
            b-stream (make-string-output-stream)) =>  #<String Output Stream>
    (format (make-broadcast-stream a-stream b-stream)
              "this will go to both streams") =>  NIL
    (get-output-stream-string a-stream) =>  "this will go to both streams"
    (get-output-stream-string b-stream) =>  "this will go to both streams"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何一个 stream 不是一个输出流就会发出一个 type-error 类型的错误.

* 也见(See Also):

        broadcast-stream-streams

* 注意(Notes): None. 



### <span id="F-MAKE-TWO-WAY-STREAM">函数 MAKE-TWO-WAY-STREAM</span>

* 语法(Syntax):

        make-two-way-stream input-stream output-stream => two-way-stream

* 参数和值(Arguments and Values):

        input-stream---一个流.
        output-stream---一个流.
        two-way-stream---一个 two-way-stream 双向流.

* 描述(Description):

        返回一个从 input-stream 得到输入并发送输出到 output-stream 的双向流.

* 示例(Examples):

    ```LISP
    (with-output-to-string (out)
        (with-input-from-string (in "input...")
          (let ((two (make-two-way-stream in out)))
            (format two "output...")
            (setq what-is-read (read two))))) =>  "output..."
    what-is-read =>  INPUT... 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

    如果 input-stream 不是一个输入流就应该发出一个 type-error 类型的错误. 如果 output-stream 不是一个输出流就应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-T-W-S-INPUT-AND-OUTPUT-STREAM">函数 TWO-WAY-STREAM-INPUT-STREAM, TWO-WAY-STREAM-OUTPUT-STREAM</span>

* 语法(Syntax):

        two-way-stream-input-stream two-way-stream => input-stream

        two-way-stream-output-stream two-way-stream => output-stream

* 参数和值(Arguments and Values):

        two-way-stream---一个 two-way-stream 双向流.
        input-stream---一个输入流.
        output-stream---一个输出流.

* 描述(Description):

        two-way-stream-input-stream 返回 two-way-stream 接收输入的那个流.

        two-way-stream-output-stream 返回 two-way-stream 发送输出的那个流.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-ECHO-STREAM-INPUT-AND-OUTPUT-STREAM">函数 ECHO-STREAM-INPUT-STREAM, ECHO-STREAM-OUTPUT-STREAM</span>

* 语法(Syntax):

        echo-stream-input-stream echo-stream => input-stream

        echo-stream-output-stream echo-stream => output-stream

* 参数和值(Arguments and Values):

        echo-stream---一个回音流.
        input-stream---一个输入流.
        output-stream---一个输出流.

* 描述(Description):

        echo-stream-input-stream 返回 echo-stream 接收输入的那个输入流.

        echo-stream-output-stream 分拣 echo-stream 发送输出的那个输出流.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MAKE-ECHO-STREAM">函数 MAKE-ECHO-STREAM</span>

* 语法(Syntax):

        make-echo-stream input-stream output-stream => echo-stream

* 参数和值(Arguments and Values):

        input-stream---一个输入流.
        output-stream---一个输出流.
        echo-stream---一个回音流.

* 描述(Description):

        创建并返回一个从 input-stream 获取输入并发送输出到 output-stream 的回音流.

* 示例(Examples):

```LISP
 (let ((out (make-string-output-stream)))
    (with-open-stream 
        (s (make-echo-stream
            (make-string-input-stream "this-is-read-and-echoed")
            out))
      (read s)
      (format s " * this-is-direct-output")
      (get-output-stream-string out)))
=>  "this-is-read-and-echoed * this-is-direct-output"
```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        echo-stream-input-stream, echo-stream-output-stream, make-two-way-stream

* 注意(Notes): None. 


### <span id="F-CONCATENATED-STREAM-STREAMS">函数 CONCATENATED-STREAM-STREAMS</span>

* 语法(Syntax):

        concatenated-stream-streams concatenated-stream => streams

* 参数和值(Arguments and Values):

        concatenated-stream -- 一个连接流.
        streams---一个输入流列表.

* 描述(Description):

        返回组成连接流 concatenated-stream 仍然需要读取的有序流集 streams 的输入流列表, 从当前读取的流开始. 如果没有更多的流被读取, 这个列表可能是空的.

        如果这些流 streams 的列表结构被修改, 那么后果是未定义的.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-MAKE-CONCATENATED-STREAM">函数 MAKE-CONCATENATED-STREAM</span>

* 语法(Syntax):

        make-concatenated-stream &rest input-streams => concatenated-stream

* 参数和值(Arguments and Values):

        input-stream---一个输入流.
        concatenated-stream---一个连接流.

* 描述(Description):

        返回一个连接流, 它具有最初与之关联的那些输入流 input-streams.

* 示例(Examples):

    ```LISP
    (read (make-concatenated-stream
            (make-string-input-stream "1")
            (make-string-input-stream "2"))) =>  12
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何一个参数不是一个输入流就应该发出 type-error.

* 也见(See Also):

        concatenated-stream-streams

* 注意(Notes): None. 


### <span id="F-GET-OUTPUT-STREAM-STRING">函数 GET-OUTPUT-STREAM-STRING</span>

* 语法(Syntax):

        get-output-stream-string string-output-stream => string

* 参数和值(Arguments and Values):

        string-output-stream---一个流.
        string---一个字符串.

* 描述(Description):

        返回一个字符串, 其中按顺序包含所有已输出到 string-output-stream 的字符. 这个操作清楚 string-output-stream 上的任何字符, 所有这个字符串 string 只包含那些从最后一次对 get-output-stream-string 的调用开始或者从 string-output-stream 创建开始已经被输出的那些字符, 取最近发生的那种情况.

* 示例(Examples):

    ```LISP
    (setq a-stream (make-string-output-stream)
            a-string "abcdefghijklm") =>  "abcdefghijklm"
    (write-string a-string a-stream) =>  "abcdefghijklm"
    (get-output-stream-string a-stream) =>  "abcdefghijklm"
    (get-output-stream-string a-stream) =>  ""
    ```

* 副作用(Side Effects):

        这个 string-output-stream 会被清理.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream-output-string 被关闭, 那么后果是未定义的.

        如果 string-output-stream 是一个不是由 make-string-output-stream 产生的流, 那么后果是未定义的. 如果 string-output-stream 是由 with-output-to-string 或 format 隐式创建的, 那么后果是未定义的.

* 也见(See Also):

        make-string-output-stream

* 注意(Notes): None. 


### <span id="F-MAKE-STRING-INPUT-STREAM">函数 MAKE-STRING-INPUT-STREAM</span>

* 语法(Syntax):

        make-string-input-stream string &optional start end => string-stream

* 参数和值(Arguments and Values):

        string---一个字符串.
        start, end---string 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        string-stream---一个输入字符串流.

* 描述(Description):

        返回一个输入字符串流. 这个流会依次提供 string 中由 start 和 end 限定的子字符串的字符. 在最后一个字符被提供之后, 这个字符串流会到达文件的末尾.

* 示例(Examples):

    ```LISP
    (let ((string-stream (make-string-input-stream "1 one ")))
      (list (read string-stream nil nil)
            (read string-stream nil nil)
            (read string-stream nil nil)))
    =>  (1 ONE NIL)

    (read (make-string-input-stream "prefixtargetsuffix" 6 12)) =>  TARGET
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        with-input-from-string

* 注意(Notes): None. 


### <span id="F-MAKE-STRING-OUTPUT-STREAM">函数 MAKE-STRING-OUTPUT-STREAM</span>

* 语法(Syntax):

        make-string-output-stream &key element-type => string-stream

* 参数和值(Arguments and Values):

        element-type---一个类型指定负. 默认是 character.
        string-stream---一个输出字符串流.

* 描述(Description):

        返回一个接收字符的输出字符串流, 并提供一个包含实际输出字符的字符串 (通过 get-output-stream-string).

        这个元素类型 element-type 命名这个字符串元素的类型; 一个字符串由可以容纳 element-type 元素的最具体的类型构成.

* 示例(Examples):

    ```LISP
    (let ((s (make-string-output-stream)))
      (write-string "testing... " s)
      (prin1 1234 s)
      (get-output-stream-string s))
    =>  "testing... 1234"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        get-output-stream-string, with-output-to-string

* 注意(Notes): None. 


### <span id="M-WITH-INPUT-FROM-STRING">宏 WITH-INPUT-FROM-STRING</span>

* 语法(Syntax):

        with-input-from-string (var string &key index start end) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        var---一个变量名.
        string---一个表达式形式; 求值来产生一个字符串.
        index---一个 place.
        start, end---string 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式 progn.
        result---由这些表达式形式 forms 返回的值.

* 描述(Description):

        创建一个输入字符串流, 提供一个时机来执行在这个流上的操作 (返回 zero 更多的值), 任何关闭这个字符串流.

        string 首先被求值, 并且 var 被绑定为一个字符输入字符串流, 它从由 start 和 end 限定的字符串的子序列中提供字符. 主体部分作为一个隐式 progn 求值.

        在从 with-input-from-string 退出时这个输入字符串流会自动关闭, 不管这个退出是正常的还是反常的. 变量 var 绑定的输入字符串流有着动态范围; 它的范围在这个表达式形式退出时结束.

        这个 index 是这个要被推进的字符串 string 中的一个指针. 如果 with-input-from-string 正常退出, 那么 index 的值就是字符串 string 中表示第一个没有读取的字符的索引, 如果所有字符都使用了那么它就是 (length string). 由 index 指定的 place 不会随着读取进度而更新, 但是只有在这个操作的结尾更新.

        start 和 index 可能都执行同一个变量, 它是要被推进的字符串 string 中的一个指针, 可能会被一些包含循环的指针重复 which is a pointer within the string to be advanced, perhaps repeatedly by some containing loop.<!--TODO 待校对-->

        如果尝试去对变量 var 赋值, 那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (with-input-from-string (s "XXX1 2 3 4xxx"
                                :index ind
                                :start 3 :end 10)
        (+ (read s) (read s) (read s))) =>  6
    ind =>  9
    (with-input-from-string (s "Animal Crackers" :index j :start 6)
      (read s)) =>  CRACKERS
    ```

        变量 j 会被设置为 15.

* 副作用(Side Effects):

        由 index 命名的 place 如果存在的话, 值会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        make-string-input-stream, 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="M-WITH-OUTPUT-TO-STRING">宏 WITH-OUTPUT-TO-STRING</span>

* 语法(Syntax):

        with-output-to-string (var &optional string-form &key element-type) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        var---一个变量名字.
        string-form---一个表达式形式或 nil; 如果不是 nil, 求值产生字符串 string.
        string---一个有着填充指针的字符串.
        element-type---一个类型指定负; 求值的. 默认是 character.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式 progn.
        results---如果没有提供一个 string-form 或者是 nil, 就是一个字符串; 否则, 就是这些表达式形式 forms 返回的值.

* 描述(Description):

        with-output-to-string 创建一个字符输出流, 执行可能发送结果到这个流的一系列操作, 然后关闭这个流.

        这个元素类型 element-type 命名这个流的元素类型; 一个流由可以容纳给定类型的元素的最具体的类型构成.

        这个主体作为一个隐式的 progn 被执行, 其中 var 被绑定为一个输出字符串流. 所有到那个字符串流的输出都会保存到一个字符串中.

        如果提供了 string, 那么 element-type 会被忽略, 并且这个输出会被递增地追加给这个字符串 string, 就像是通过 vector-push-extend 一样.

        这个输出流在从 with-output-from-string 退出时会自动关闭, 不管这个退出是正常的还是异常的. 这个绑定给变量 var 的输出字符串流有着动态范围; 它的范围在退出这个表达式形式时终止.

        如果没有提供 string, 那么 with-output-from-string 产生一个接收字符的流并返回一个指定的 element-type 的字符串. 如果提供了 string, with-output-to-string 返回求值最后一个表达式形式的结果.

        如果尝试去对变量 var 赋值, 那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (setq fstr (make-array '(0) :element-type 'base-char
                                :fill-pointer 0 :adjustable t)) =>  ""
    (with-output-to-string (s fstr)
        (format s "here's some output")
        (input-stream-p s)) =>  false
    fstr =>  "here's some output"
    ```

* 副作用(Side Effects):

        这个 string 会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果在这个调用的动态范围内直接执行 string 上的破坏性修改, 那么后果是未定义的.

* 也见(See Also):

        make-string-output-stream, vector-push-extend, 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="V-IO-ALL">变量 *DEBUG-IO*, *ERROR-OUTPUT*, *QUERY-IO*, *STANDARD-INPUT*, *STANDARD-OUTPUT*, *TRACE-OUTPUT*</span>

* 值类型(Value Type):

        对于 *standard-input*: 一个输入流

        对于 *error-output*, *standard-output*, 和 *trace-output*: 一个输出流.

        对于 *debug-io*, *query-io*: 一个双向流.

* 初始值(Initial Value):

        依赖于具体实现的, 但是它必须是一个打开的流, 不是一个到 I/O 定制变量的广义的同义流但是可能是一个到某个 I/O 定制变量的值的广义的同义流. 这个初始值可能也是一个广义的同义流, 是符号 *terminal-io* 或者是作为它的值的流.

* 描述(Description):

        这些变量全体被称为标准 I/O 定制变量. 它们可以被绑定和赋值, 来更改各种标准化操作符和工具所使用的输入和/或输出的默认目的地.

        *debug-io* 的值, 称为调试 I/O, 是一个被用于交互式调试目的的流.

        *error-output* 的值, 称为错误输出, 是一个警告和非交互式错误信息应该被发送到的流.

        *query-io* 的值, 称为查询 I/O, 是一个在询问用户问题时使用的双向流. 这个问题应该输出到这个流中, 并且答复也从这个流读取.

        *standard-input* 的值, 称为标准输入, 是许多操作符没有明确提供特定的输入流时用作默认的输入源使用的一个流.

        *standard-output* 的值, 称为标准输出, 是许多操作符没有明确提供特定的输出流时用作默认的目的地使用的一个流.

        *trace-output* 的值, 称为跟踪输出, 是一个流, 跟踪函数(见 trace)和 time 宏打印它们的输出到这个流上.

* 示例(Examples):

    ```LISP
    (with-output-to-string (*error-output*)
      (warn "this string is sent to *error-output*"))
    =>  "Warning: this string is sent to *error-output*
    " ;The exact format of this string is implementation-dependent.

    (with-input-from-string (*standard-input* "1001")
        (+ 990 (read))) =>  1991                       

    (progn (setq out (with-output-to-string (*standard-output*)
                        (print "print and format t send things to")
                        (format t "*standard-output* now going to a string")))
            :done)
    =>  :DONE
    out
    =>  "
    \"print and format t send things to\" *standard-output* now going to a string"

    (defun fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))
    =>  FACT
    (trace fact)
    =>  (FACT)
    ;; Of course, the format of traced output is implementation-dependent.
    (with-output-to-string (*trace-output*)
      (fact 3)) 
    =>  "
    1 Enter FACT 3
    | 2 Enter FACT 2
    |   3 Enter FACT 1
    |   3 Exit FACT 1
    | 2 Exit FACT 2
    1 Exit FACT 6"
    ```

* 也见(See Also):

        *terminal-io*, synonym-stream, time, trace, 章节 9 (Conditions), 章节 23 (Reader), 章节 22 (Printer)

* 注意(Notes):

        这些 I/O 定制变量的初始值上约束的意图是去确保绑定或赋值这样一个变量到另一个 I/O 定制变量的值总是安全的, 不过度限制具体实现的灵活性.

        使 *debug-io* 和 *query-io* 的初始值为同一个流以及使 *error-output* 和 *standard-output* 的初始值为同一个流对于一个实现来说是很常见的.

        函数 y-or-n-p 和 yes-or-no-p 使用查询 I/O 作为它们的输入和输出.

        在正常的 Lisp read-eval-print 循环中, 输入是从标准输入读取的. 很多输入函数, 包括 read 和 read-char, 接收一个默认为标准输入的流参数.

        在正常的 Lisp read-eval-print 循环中, 输出被发送到标准输出. 好的输出函数, 包括 print 和 write-char, 接受默认为标准输出的参数.

        例如, 一个程序想要将输出转移到文件中, 应该通过绑定 *standard-output* 来实现; 发送到 *error-output* 的错误消息仍然可以通过 *terminal-io* 到达用户(如果 *error-output* 被绑定到 *terminal-io* 的话), 这通常是需要的. 


### <span id="V-TERMINAL-IO">变量 *TERMINAL-IO*</span>

* 值类型(Value Type):

        一个双向流.

* 初始值(Initial Value):

        依赖于具体实现的, 但是它必须是一个打开的流, 不是一个到 I/O 定制变量的广义的同义流但是可能是一个到某个 I/O 定制变量的值的广义的同义流.

* 描述(Description):

        被称为终端 I/O 的 *terminal-io* 的值通常是一个连接到用户终端的双向流. 典型地, 写入到这个流会导致输出出现在一个显示的屏幕, 比如, 从这个流读取会从键盘接收一个输入. 它的目的是, 当与这个流一起使用时, 诸如 read 和 read-char 之类的标准输入函数, 会引起输入到流的输出端的响应. 实现这一点的方法是依赖于具体实现的.

        改变 *terminal-io* 的值的效果, 不管是通过绑定还是赋值, 是具体实现定义的.

* 示例(Examples):

    ```LISP
    (progn (prin1 'foo) (prin1 'bar *terminal-io*))
    >>  FOOBAR
    =>  BAR
    (with-output-to-string (*standard-output*)
      (prin1 'foo) 
      (prin1 'bar *terminal-io*))
    >>  BAR
    =>  "FOO"
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        *debug-io*, *error-output*, *query-io*, *standard-input*, *standard-output*, *trace-output*

* 注意(Notes): None. 


### <span id="CT-STREAM-ERROR">状况类型 STREAM-ERROR</span>

* 类优先级列表(Class Precedence List):

        stream-error, error, serious-condition, condition, t

* 描述(Description):

        类型 stream-error 由从一个流接收输入或发送输出到一个流相关的错误状况构成. 这个 "违规的流" 通过 给 make-condition 的 :streaminitialization 参数来初始化, 并且通过函数 stream-error-stream 来访问.

* 也见(See Also):

        stream-error-stream 


### <span id="F-STREAM-ERROR-STREAM">函数 STREAM-ERROR-STREAM</span>

* 语法(Syntax):

        stream-error-stream condition => stream

* 参数和值(Arguments and Values):

        condition---一个 stream-error 类型的状况.
        stream---一个流.

* 描述(Description):

        返回一个 stream-error 类型的状况的违规的流.

* 示例(Examples):

    ```LISP
    (with-input-from-string (s "(FOO")
      (handler-case (read s)
        (end-of-file (c)
          (format nil "~&End of file on ~S." (stream-error-stream c)))))
    "End of file on #<String Stream>."
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        stream-error, 章节 9 (Conditions)

* 注意(Notes): None. 


### <span id="CT-END-OF-FILE">状况类型 END-OF-FILE</span>

* 类优先级列表(Class Precedence List):

        end-of-file, stream-error, error, serious-condition, condition, t

* 描述(Description):

        类型 end-of-file 包含与在没有更多数据的流中完成的读取操作相关的错误状况.

* 也见(See Also):

        stream-error-stream 


