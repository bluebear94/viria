#lang racket
#|
Common code for all modules in the /modules directory and the main program to share.
|#


(define (sl2 l x) (if (or (empty? l) (equal? x (first l))) 0
                      (add1 (sl2 (rest l) x))))
(define (search-list l x) (letrec ([i (sl2 l x)])
                            (if (>= i (length l)) -1 i)))
(define (ss2 s c) (sl2 (string->list s) c))
(define (search-string s c) (search-list (string->list s) c))

(struct call (name args) #:transparent #:mutable)
(struct dotp (expr flip) #:transparent #:mutable) ; Short for dotpivot, of course
(struct bindings (argumented argless) #:transparent #:mutable)
(struct gscv (ec cc cs rt bind gce cgce) #:transparent #:mutable)
(struct proj (name progbind proglist) #:transparent #:mutable)
(define test (gscv "HelloProgram" "HELLO" "#out HELLO\nClrHome\nDisp \"HELLO WORLD\"\nOutput(8,1,2A+Bi"
                   (list (call "hash" (list "out" "HELLO")) ; the list of tokens. ^-1, ^2, and ^3 are considered "postfix"es.
                         (call "newline" empty)
                         (call "cmd" (list "ClrHome"))
                         (call "cmd" (list "Disp"))
                         (call "stringlit" (list (call "numvar" (list #\H)) (call "numvar" (list #\E)) (call "numvar" (list #\L)) (call "numvar" (list #\L)) (call "numvar" (list #\O))
                                                 (call "space" empty)
                                                 (call "numvar" (list #\W)) (call "numvar" (list #\O)) (call "numvar" (list #\R)) (call "numvar" (list #\L)) (call "numvar" (list #\D))))
                         (call "newline" empty)
                         (call "cmdp" (list "Output"))
                         (call "numerals" (list 8))
                         (call "comma" empty)
                         (call "numerals" (list 1))
                         (call "comma" empty)
                         (call "numerals" (list 2))
                         (call "numvar" (list #\A))
                         (call "op" (list "+"))
                         (call "numvar" (list #\B))
                         (call "imunit" empty))
                   (bindings
                    (make-hash)
                    (make-hash)
                    )
                   (list (call "cmd" (list "ClrHome")) ; now parsed into a tree
                         (call "cmd" (list "Disp"
                                           (call "stringlit" (list (call "numvar" (list #\H)) (call "numvar" (list #\E)) (call "numvar" (list #\L)) (call "numvar" (list #\L)) (call "numvar" (list #\O))
                                                                   (call "space" empty)
                                                                   (call "numvar" (list #\W)) (call "numvar" (list #\O)) (call "numvar" (list #\R)) (call "numvar" (list #\L)) (call "numvar" (list #\D))))))
                         (call "cmd" (list "Output"
                                           8
                                           1
                                           (call "op"
                                                 (list "+"
                                                       (call "op"
                                                             (list ""
                                                                   2
                                                                   (call "numvar" (list #\A))))
                                                       (call "op"
                                                             (list ""
                                                                   (call "numvar" (list #\B))
                                                                   (call "imunit" empty))))))))
                   (list (call "cmd" (list "ClrHome")) ; multiple +-*/ calls unified into +. and *. calls
                         (call "cmd" (list "Disp"
                                           (call "stringlit" (list (call "numvar" (list #\H)) (call "numvar" (list #\E)) (call "numvar" (list #\L)) (call "numvar" (list #\L)) (call "numvar" (list #\O))
                                                                   (call "space" empty)
                                                                   (call "numvar" (list #\W)) (call "numvar" (list #\O)) (call "numvar" (list #\R)) (call "numvar" (list #\L)) (call "numvar" (list #\D))))))
                         (call "cmd" (list "Output"
                                           8
                                           1
                                           (call "op"
                                                 (list "+."
                                                       (dotp (call "op"
                                                                   (list "."
                                                                         (dotp 2 false)
                                                                         (dotp (call "numvar" (list #\A)) false))) false)
                                                       (dotp (call "op"
                                                                   (list "."
                                                                         (dotp (call "numvar" (list #\B)) false)
                                                                         (dotp (call "imunit" empty) false)))) false)))))))

(struct moar (x) #:transparent #:mutable)
(define regrarity (list 0 1 2 3 4))
(define none (list 0))
(define testarity (list 0 2 3 4 5))

(define cmds (hash "oVarStats" (list 0 1 2) ; This is the only command list requiring the list of possible arities. If any value x and above can be used, then you may use (moar x).
                   "tVarStats" (list 0 2 3) ; 
                   "tSampFTest" (moar 0)
                   "tSampTInt" (moar 0)
                   "tSampTTest" (moar 0)
                   "Archive" (list 1) ; If they take arguments, then such functions will search for a space before the first argument, and commas before the rest.
                   "AUTOAnswer" none
                   "AxesOff" none ; Note: some commands have different names to please the lexer
                   "AxesOn" none
                   "apbi" none
                   "Boxplot" none
                   "CLASSIC" none
                   "ClearEntries" none
                   "ClockOff" none
                   "ClockOn" none
                   "ClrAllLists" none
                   "ClrDraw" none
                   "ClrHome" none
                   "ClrList" (moar 1)
                   "ClrTable" none
                   "Connected" none
                   "CoordOff" none
                   "CoordOn" none
                   "CubicReg" regrarity
                   "DECAnswer" none
                   "Degree" none
                   "DependAsk" none
                   "DependAuto" none
                   "DiagnosticOff" none
                   "DiagnosticOn" none
                   "Disp" (moar 0)
                   "DrawF" (list 1)
                   "DrawInv" (list 1)
                   "\\e" (list 1) ; this is the small e
                   "Else" none
                   "End" none
                   "Eng" none
                   "ExecLib" none
                   "ExpReg" regrarity
                   "ExprOff" none
                   "ExprOn" none
                   "Fix" (list 1)
                   "Float" none
                   "FnOff" (moar 0)
                   "FnOn" (moar 0)
                   "FRAC Answer" none
                   "Full" none
                   "Func" none
                   "GarbageCollect" none
                   "getDate" none
                   "getDtFmt" none
                   "getKey" none
                   "getTime" none
                   "getTmFmt" none
                   ; Lbl and Goto have their own calls
                   "GridOff" none
                   "GridOn" none
                   "GT" none
                   "Histogram" none
                   "Horiz" none
                   "Horizontal" (list 1)
                   "If" (list 1)
                   "IndpntAsk" none
                   "IndpntAuto" none
                   "Input" (list 0 1 2)
                   "isClockOn" none
                   "LabelOff" none
                   "LabelOn" none
                   "LinRegABX" none
                   "LinRegAXB" none
                   "LinRegTInt" testarity
                   "LinRegTTest" testarity
                   "LnReg" regrarity
                   "Logistic" regrarity
                   "ManualFit" (list 0 1)
                   "MATHPRINT" none
                   "Med-Med" regrarity
                   "ModBoxPlot" none
                   "Normal" none
                   "NormProbPlot" none
                   "Param" none
                   "Pause" (list 0 1)
                   "PlotsOff" (moar 0)
                   "PlotsOn" (moar 0)
                   "Pmt_Bgn" none
                   "Pmt_End" none
                   "Polar" none
                   "PolarGC" none
                   ; Both exec and prgm have their own calls
                   "Prompt" (moar 1)
                   "PowerReg" regrarity
                   "QuadReg" regrarity
                   "QuartReg" regrarity
                   "Radian" none
                   "Reti" none
                   "Real" none
                   "RecallGDB" (list 1)
                   "RecallPic" (list 1)
                   "RectGC" none
                   "Repeat" (list 1)
                   "Return" none
                   "Scatter" none
                   "Sci" none
                   "Seq" none
                   "Sequential" none
                   "SetUpEditor" (moar 0)
                   "Simul" none
                   "SinReg" regrarity
                   "startTmr" none
                   "Stop" none
                   "StoreGDB" (list 1)
                   "StorePic" (list 1)
                   "Then" none
                   "Time" none
                   "TInterval" (moar 0)
                   "Trace" none
                   "TTest" (moar 0)
                   "UnArchive" (list 1)
                   "uvAxes" none
                   "uwAxes" none
                   "Vertical" (list 1)
                   "vwAxes" none
                   "Web" none
                   "While" (list 1)
                   "xyLine" none
                   "ZBox" none
                   "ZDecimal" none
                   "ZInteger" none
                   "ZInterval" none
                   "ZoomIn" none
                   "ZoomOut" none
                   "ZoomFit" none
                   "ZoomRcl" none
                   "ZoomStat" none
                   "ZoomSto" none
                   "ZPrevious" none
                   "ZSquare" none
                   "ZStandard" none
                   ; note: Z-Test has a parenthesis attached
                   "ZTrig" none
                   ))
(define cmdps (hash "sqrt" (list 1)
                    "cbrt" (list 1)
                    ; xrt is an operator
                    "oPropZInt" (moar 0)
                    "tPropZInt" (moar 0)
                    "oPropZTest" (moar 0)
                    "tPropZTest" (moar 0)
                    "tSampZInt" (moar 0)
                    "tSampZTest" (moar 0)
                    "abs" (list 1)
                    "angle" (list 1)
                    "ANOVA" (moar 1)
                    "Asm" (list 1)
                    "AsmComp" (list 2)
                    "augment" (list 2)
                    "bal" (list 1 2)
                    "binomcdf" (list 2 3)
                    "binompdf" (list 2 3)
                    "checkTmr" (list 1)
                    "chi2cdf" (list 3)
                    "chi2pdf" (list 3)
                    "chi2Test" (moar 0)
                    "chi2GOFTest" (moar 0)
                    "Circle" (list 3 4)
                    "conj" (list 1)
                    "cos" (list 1)
                    "arccos" (list 1)
                    "cosh" (list 1)
                    "arccosh" (list 1)
                    "cot" (list 1)
                    "arccot" (list 1)
                    "coth" (list 1)
                    "arccoth" (list 1)
                    "csc" (list 1)
                    "arccsc" (list 1)
                    "csch" (list 1)
                    "arccsch" (list 1)
                    "cumul" (list 1)
                    "dayOfWk" (list 3)
                    "dbd" (list 2)
                    "det" (list 1)
                    "dim" (list 1) ; note: to resize an array, use resize()
                    "DSL" (list 2)
                    "exp" (list 1)
                    "toEff" (list 2)
                    "EquToString" (list 2)
                    "expr" (list 1)
                    "Fcdf" (list 4)
                    "Fill" (list 2)
                    "fMax" (list 4 5)
                    "fMin" (list 4 5)
                    "fnInt" (list 4 5)
                    "For" (list 3 4)
                    "fPart" (list 1)
                    "Fpdf" (list 3)
                    "gcd" (list 2)
                    "geometcdf" (list 2)
                    "geometpdf" (list 2)
                    "Get" (list 1)
                    "GetCalc" (list 1 2)
                    "getDtStr" (list 1)
                    "getTmStr" (list 1)
                    "GraphStyle" (list 2)
                    "identity" (list 1)
                    "imag" (list 1)
                    "inString" (list 2 3)
                    "int" (list 1)
                    "Sint" (list 2 3)
                    "invNorm" (list 1 3)
                    "invT" (list 2)
                    "iPart" (list 1)
                    "irr" (list 2 3)
                    "ISG" (list 2)
                    "lcm" (list 2)
                    "length" (list 1)
                    "Line" (list 4 5)
                    "dList" (list 1)
                    "ListToMatr" (moar 2)
                    "ln" (list 1)
                    "log" (list 1)
                    "logBase" (list 2)
                    "MatrToList" (moar 2)
                    "max" (list 1 2)
                    "mean" (list 1 2)
                    "median" (list 1 2)
                    "Menu" (list 3 5 7 9 11 13 15)
                    "min" (list 1 2)
                    "nDeriv" (list 3 4)
                    "toNom" (list 2)
                    "normalcdf" (list 2 4)
                    "normalpdf" (list 1 3)
                    "not" (list 1)
                    "npv" (list 3 4)
                    "OpenLib" (list 1)
                    "Output" (list 3)
                    "Plot1" (list 4)
                    "Plot2" (list 4)
                    "Plot3" (list 4)
                    "poissoncdf" (list 2)
                    "poissonpdf" (list 2)
                    "Sprn" (list 2 3)
                    "prod" (list 1 2 3)
                    "PtChange" (list 2)
                    "PtOff" (list 2 3)
                    "PtOn" (list 2 3)
                    "PxlChange" (list 2)
                    "PxlOff" (list 2)
                    "PxlOn" (list 2)
                    "pxlTest" (list 2)
                    "PToRx" (list 2)
                    "PToRy" (list 2)
                    "rand" (list 0 1)
                    "randBin" (list 2 3)
                    "randInt" (list 2 3)
                    "randIntNoRep" (list 2)
                    "randM" (list 2)
                    "randNorm" (list 2 3)
                    "real" (list 1)
                    "ref" (list 1)
                    "remain" (list 2)
                    "round" (list 1 2)
                    "mRow" (list 3)
                    "mRowA" (list 4)
                    "RowA" (list 3)
                    "rowSwap" (list 3)
                    "rref" (list 1)
                    "RToPr" (list 2)
                    "RToPt" (list 2)
                    "resize" (list 2)
                    "Select" (list 2)
                    "Send" (list 1)
                    "seq" (list 4 5)
                    "setDate" (list 3)
                    "setDtFmt" (list 1)
                    "setTime" (list 3)
                    "setTmFmt" (list 1)
                    "Shade" (list 2 6)
                    "ShadeChi2" (list 3)
                    "ShadeF" (list 4)
                    "ShadeNorm" (list 2 4)
                    "Shade_t" (list 3)
                    "sec" (list 1)
                    "arcsec" (list 1)
                    "sech" (list 1)
                    "arcsech" (list 1)
                    "sin" (list 1)
                    "arcsin" (list 1)
                    "sinh" (list 1)
                    "arcsinh" (list 1)
                    "solve" (list 3 4)
                    "SortA" (moar 1)
                    "SortD" (moar 1)
                    "stdDev" (list 1 2)
                    ; StrToEqu? Use :=
                    "sub" (list 3)
                    "sum" (list 1 2 3)
                    "Sigma" (list 3)
                    "tan" (list 1)
                    "arctan" (list 1)
                    "tanh" (list 1)
                    "arctanh" (list 1)
                    "Tangent" (list 2)
                    "tcdf" (list 3)
                    "tpdf" (list 2)
                    "Text" (moar 3)
                    "timeCnv" (list 1)
                    "tvm_FV" (moar 0)
                    "tvm_I%" (moar 0)
                    "tvm_N" (moar 0)
                    "tvm_Pmt" (moar 0)
                    "tvm_PV" (moar 0)
                    "variance" (list 1 2)
                    "ZTest" (moar 0)
                    )) ; with a left parenthesis attached to the beginning
(define postfixes (list "!" "%" "degrees" "radians" "^-1" "^2" "^3" "toDec" "toDMS" "toFD" "toFrac" "toPolar" "toNDUND" "toRect"))
(define ops (list "+" "-" "*" "/" "^" "xrt" "==" "!=" "=" "<" ">" "<=" ">=" "&&" "||" "^^" "\\/" "\\u" "++"
                  "nPr" "nCr"))
(define syms (hash "Xmin" #x630a
                   "Xmax" #x630b
                   "Ymin" #x630c
                   "Ymax" #x630d
                   "dX" #x6326
                   "dY" #x6327
                   "Xscl" #x6302
                   "Yscl" #x6303
                   "Xres" #x6336
                   "Tmin" #x630e
                   "Tmax" #x630f
                   "Tstep" #x6322
                   "tmin" #x6310
                   "tmax" #x6311
                   "tstep" #x6323
                   "nMin" #x631f
                   "nMax" #x631d
                   "unMin" #x6304
                   "vnMin" #x6305
                   "wnMin" #x6332
                   "PlotStart" #x631b
                   "PlotStep" #x6334
                   "ZXmin" #x6312
                   "ZXmax" #x6313
                   "ZYmin" #x6314
                   "ZYmax" #x6315
                   ;"ZdX"
                   ;"ZdY"
                   "ZXscl" #x6300
                   "ZYscl" #x6301
                   "ZXres" #x6337
                   "ZTmin" #x6318
                   "ZTmax" #x6319
                   "ZTstep" #x6324
                   "Ztmin" #x6316
                   "Ztmax" #x6317
                   "Ztstep" #x6325
                   "ZnMin" #x6320
                   "ZnMax" #x631e
                   "ZunMin" #x6308
                   "ZvnMin" #x6309
                   "ZwnMin" #x6333
                   "ZPlotStart" #x631c
                   "ZPlotStep" #x6335
                   "XFact" #x6328
                   "YFact" #x6329
                   "TblStart" #x631a
                   "dTbl" #x6321
                   "TblInput" #x632a
                   "\\N" #x632b
                   "I%" #x632c
                   "PV" #x632d
                   "PMT" #x632e
                   "FV" #x632f
                   "PpY" #x6330
                   "CpY" #x6331
                   "\\n" #x6202
                   "xb" #x6203
                   "yb" #x620c
                   "Sx" #x6204
                   "Sxs" #x6205
                   "Sy" #x620d
                   "Sys" #x620e
                   "Sxy" #x6211
                   "ox"  #x6207
                   "sx" #x6206
                   "oy" #x6210
                   "sy" #x620f
                   "minX" #x6208
                   "maxX" #x6209
                   "minY" #x620a
                   "maxY" #x620b
                   "\\r" #x6212
                   "Med" #x6213
                   "RegEq" #x6201
                   "Q1" #x6214
                   "Q3" #x6215
                   "\\a" #x6216
                   "\\b" #x6217
                   "\\c" #x6218
                   "\\d" #x6219
                   "\\e" #x621a
                   "x_1" #x621b
                   "x_2" #x621c
                   "x_3" #x621d
                   "y_1" #x621e
                   "y_2" #x621f
                   "y_3" #x6220
                   "nn" #x6221
                   "\\p" #x6222
                   "\\z" #x6223
                   "\\t" #x6224
                   "chi2" #x6225
                   "\\F" #x6226
                   "df" #x6227
                   "phat" #x6228
                   "phat1" #x6229
                   "phat2" #x622a
                   "xb1" #x622b
                   "Sx1" #x622c
                   "n1" #x622d
                   "xb2" #x622e
                   "Sx2" #x622f
                   "n2" #x6230
                   "Sxp" #x6231
                   "lower" #x6232
                   "upper" #x6233
                   "\\s" #x6234
                   "r2" #x6235
                   "R2" #x6236
                   "Factordf" #x6237
                   "FactorSS" #x6238
                   "FactorMS" #x6239
                   "Errordf" #x623a
                   "ErrorSS" #x623b
                   "ErrorMS" #x623c)) ; system variables
(define aliases (hash "RclPic" "RecallPic"
                      "RclGDB" "RecallGDB"
                      "ZStd" "ZStandard"
                      "and" "&&"
                      "or" "||"
                      "xor" "^^"
                      )) ; command aliases


(define (delimiter? c)
  (not (= -1 (ss2 " +-*/^()[]{}!,=<>»:" c))))

(define (next-delimiter s)
  (cond [(= 0 (string-length s)) 0]
        [(delimiter? (string-ref s 0)) 0]
        [else (add1 (next-delimiter (substring s 1)))]))

(provide (all-defined-out))