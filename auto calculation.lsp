(defun C:calc () 
  (vl-load-com)
  (setvar "DIMZIN" 0) ;精度補0
  (setvar "osmode" 0) ;取消鎖點
  (setvar "cmdecho" 0) ;指令執行過程不回應
  ;;-------------------**副程式**--------------------;;
  ;;-------------------**副程式**--------------------;;
  ;;-------------------**副程式**--------------------;;
  ;;-------------------**副程式**--------------------;;
  ;;-------------------坵塊圖塊元素---------------------
  (defun rectdata (OBJ) 
    (setq OBJ (vlax-ename->vla-object OBJ))
    (vlax-for X 
      (vla-item 
        (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
        (vla-get-name OBJ)
      )
      (setq lst (cons (vlax-vla-object->ename X) lst))
    )
  )
  ;;-------------------坵塊圖塊元素end------------------------
  ;;-------------------交點------------------------
  (defun getinters (En_A En_B) 
    ;(SETQ En_A (car (ENTSEL "\n 選取物件 A")))
    ;(SETQ En_B (car (ENTSEL "\n 選取物件 B")))
    (SETQ OBJ_A (VLAX-ENAME->VLA-OBJECT En_A))
    (SETQ OBJ_B (VLAX-ENAME->VLA-OBJECT En_B))
    (setq int nil)
    (setq INTLST (vl-catch-all-apply 
                   'vlax-safearray->list
                   (list 
                     (vlax-variant-value 
                       (vla-intersectwith OBJ_A OBJ_B acextendnone)
                     )
                   )
                 )
    )

    (IF (vl-catch-all-error-p INTLST) 
      (SETQ INT nil)
      (SETQ INT intlst)
    )
  )
  ;;-------------------交點end------------------------
  ;;------------------填入文字------------------------
  (defun inserttext () 
    ;;------------------------一般聚合線
    ;;------求等高線資料
    (setq all1mline (ssget "x" 
                           (cons line1mlayer 
                                 '((-4 . "<OR")
                                   (0 . "LWPOLYLINE")
                                   (0 . "POLYLINE")
                                   (-4 . "OR>")
                                  )
                           )
                    )
    )
    (setq all5mline (ssget "x" 
                           (cons line5mlayer 
                                 '((-4 . "<OR")
                                   (0 . "LWPOLYLINE")
                                   (0 . "POLYLINE")
                                   (-4 . "OR>")
                                  )
                           )
                    )
    )
    (setq 1mnum (sslength all1mline))
    (setq 5mnum (sslength all5mline))
    (command "_pedit" "m" all1mline "" "f" "") ;把聚合線 "d直線化" "f擬合"
    (command "_pedit" "m" all5mline "" "f" "") ;把聚合線 "d直線化" "f擬合"
    ;;------求等高線資料end
    ;;------求交點數-----------
    (setq intcoor (list))
    (setq line0 0)
    (setq alls (list))
    ;;---方型角3 4號座標串列
    (setq rectang34 (list))
    ;;------1M
    (repeat rectnum 
      (setq line 0)
      (setq ena (ssname allrect line0))
      (repeat 1mnum 
        (setq enb (ssname all1mline line))
        (setq enc (VLAX-ENAME->VLA-OBJECT enb)) ;vla 線data
        (setq end (VLAX-ENAME->VLA-OBJECT ena)) ;vla 方data
        (setq high0 (vla-get-elevation enc))
        (vla-put-elevation enc 0) ;線高程歸0
        (vla-put-elevation end 0) ;方高程歸0
        (setq line (+ line 1))
        (getinters ena enb)
        (vla-put-elevation enc high0)
        (if (= int nil) 
          (setq int nil)
          (setq intcoor (append int intcoor))
        )
      ) ;repeat
      ;;------5M
      (setq line 0)
      (repeat 5mnum 
        (setq enb (ssname all5mline line))
        (setq enc (VLAX-ENAME->VLA-OBJECT enb)) ;vla 線data
        (setq end (VLAX-ENAME->VLA-OBJECT ena)) ;vla 方data
        (setq high0 (vla-get-elevation enc))
        (vla-put-elevation enc 0) ;線高程歸0
        (vla-put-elevation end 0) ;方高程歸0
        (setq line (+ line 1))
        (getinters ena enb)
        (vla-put-elevation enc high0)
        (if (= int nil) 
          (setq int nil)
          (setq intcoor (append int intcoor))
        )
      ) ;repeat
      (setq allintnum (/ (length intcoor) 3)) ;;計算交點數
      ;;------------------填入文字
      ;;新增文字圖層
      (setq ly (tblsearch "layer" "坡度分析文字"))
      (if (= ly nil) 
        (command ".-layer" "n" "坡度分析文字" "c" "30" "坡度分析文字" "")
        (command ".-layer" "n" "坡度分析坡度" "c" "30" "坡度分析坡度" "")
      )
      (setvar "clayer" "坡度分析文字")


      ;;方形座標
      (setq rectdata (entget ena))
      (setq textcoor1 (assoc 10 rectdata)) ;;方形左下座標
      (setq rectdata (subst '(-2 0.0 0.0 0.0) textcoor1 rectdata))
      (setq textcoor1 (cdr textcoor1))
      (setq textcoor2 (assoc 10 rectdata)) ;;方形右下座標
      (setq rectdata (subst '(-2 0.0 0.0 0.0) textcoor2 rectdata))
      (setq textcoor2 (cdr textcoor2))
      (setq textcoor3 (assoc 10 rectdata)) ;;方形右上座標
      (setq rectdata (subst '(-2 0.0 0.0 0.0) textcoor3 rectdata))
      (setq textcoor3 (cdr textcoor3))
      (setq textcoor4 (cdr (assoc 10 rectdata))) ;;方形左上座標
      ;;求長度
      (setq width (distance textcoor1 textcoor2))
      (setq scale (/ width gw)) ;;縮放比例
      ;;方形型心
      (setq textcoorm (list (/ (+ (car textcoor1) (car textcoor3)) 2) 
                            (/ (+ (cadr textcoor1) (cadr textcoor3)) 2)
                      )
      )
      ;;方形下or下方中點
      (setq textcoorbmm (list (/ (+ (car textcoor1) (car textcoor2)) 2) 
                              (/ (+ (cadr textcoor1) (cadr textcoor2)) 2)
                        )
      )
      ;;方形上or下方中點比較
      (setq textcoortmm (list (/ (+ (car textcoor3) (car textcoor4)) 2) 
                              (/ (+ (cadr textcoor3) (cadr textcoor4)) 2)
                        )
      )
      (if (> (- (cadr textcoortmm) (cadr textcoorbmm)) 0) 
        (setq textcoortm textcoortmm
              textcoorbm textcoorbmm
        )
        (setq textcoortm textcoorbmm
              textcoorbm textcoortmm
        )
      )
      ;;方形右方中點
      (setq textcoorrmm (list (/ (+ (car textcoor2) (car textcoor3)) 2) 
                              (/ (+ (cadr textcoor2) (cadr textcoor3)) 2)
                        )
      )
      ;;方形右or左方中點比較
      (setq textcoorlmm (list (/ (+ (car textcoor1) (car textcoor4)) 2) 
                              (/ (+ (cadr textcoor1) (cadr textcoor4)) 2)
                        )
      )
      (if (> (- (cadr textcoorrmm) (cadr textcoorlmm)) 0) 
        (setq textcoorrm textcoorrmm
              textcoorlm textcoorlmm
        )
        (setq textcoorrm textcoorlmm
              textcoorlm textcoorrmm
        )
      )
      ;;計算那邊為最上方邊
      (if (> (- (cadr textcoortm) (cadr textcoorrm)) 0) 
        (setq textcoortm textcoortm
              textcoorbm textcoorbm
        )
        (setq textcoortm textcoorrm
              textcoorbm textcoorlm
        )
      )
      (setq rectrad (- (angle textcoorbm textcoortm) (* pi 0.5))) ;;計算角度
      ;;填入N值
      (setq textcoorxy (list (/ (+ (car textcoorbm) (car textcoortm)) 2) 
                             (/ (+ (cadr textcoorbm) (cadr textcoortm)) 2)
                       )
      )
      (setq textcoorxy (list (/ (+ (car textcoorxy) (car textcoortm)) 2) 
                             (/ (+ (cadr textcoorxy) (cadr textcoortm)) 2)
                       )
      )
      (setq text (strcat "(n=" (itoa allintnum) ")"))
      (command "-text" 
               "j"
               "m"
               textcoorxy
               (* texth scale)
               (/ (* rectrad 180) pi)
               text
      ) ;;(/ (* rectrad 180) pi)角度
      ;;填入S值
      (setq svalue (/ (* allintnum pi deltah 100) (* width 8)))
      (setq alls (cons svalue alls)) ;;所有坡度的list
      (setq text (strcat "S=" (rtos svalue 2 2) "%"))
      (command "-text" 
               "j"
               "m"
               textcoorm
               (* texth scale)
               (/ (* rectrad 180) pi)
               text
      )
      ;;填入坡度

      (setq textcoorxy (list (/ (+ (car textcoorbm) (car textcoortm)) 2) 
                             (/ (+ (cadr textcoorbm) (cadr textcoortm)) 2)
                       )
      )
      ;;坡度值座標
      (setvar "clayer" "坡度分析坡度")
      (setq textcoorxy (list (/ (+ (car textcoorxy) (car textcoorbm)) 2) 
                             (/ (+ (cadr textcoorxy) (cadr textcoorbm)) 2)
                       )
      )
      (cond 
        ((<= svalue 5) (setq text 1))
        ((<= svalue 15) (setq text 2))
        ((<= svalue 30) (setq text 3))
        ((<= svalue 40) (setq text 4))
        ((<= svalue 55) (setq text 5))
        ((<= svalue 100) (setq text 6))
        ((> svalue 100) (setq text 7))
      )
      (command "-text" 
               "j"
               "m"
               textcoorxy
               (* texth scale)
               (/ (* rectrad 180) pi)
               text
      )
      ;;坡度顏色hatch
      (progn 
        (command "_layiso" ena "")
        (command ".-layer" "n" "坡度hatch" "c" "1" "坡度hatch" "") ;;建立輔助線圖層
        (setvar "clayer" "坡度hatch")
        (command "-hatch" "p" "s" "")
        (cond 
          ((<= text 3)
           (command "-hatch" "p" "s" "dr" "b" "t" "60" "co" "3" "A" "B" "e" "" 
                    textcoorxy ""
           )
          )
          ((<= text 5)
           (command "-hatch" "p" "s" "dr" "b" "t" "60" "co" "30" "A" "B" "e" "" 
                    textcoorxy ""
           )
          )
          ((<= text 7)
           (command "-hatch" "p" "s" "dr" "b" "t" "60" "co" "1" "A" "B" "e" "" 
                    textcoorxy ""
           )
          )
        ) ;;cond
        (command "_layuniso")
      ) ;progn
      ;;坡度顏色hatch end
      (setvar "clayer" "坡度分析文字")
      ;;填入坵塊編號 左上11 右上22 左下44 右下33
      (cond 
        ((= textcoortm textcoortmm)
         (if (> (car textcoortmm) (car textcoor4)) 
           (if (> (car textcoorbmm) (car textcoor1)) 
             (setq textcoor11 textcoor4
                   textcoor22 textcoor3
                   textcoor33 textcoor2
                   textcoor44 textcoor1
             )
             (setq textcoor11 textcoor4
                   textcoor22 textcoor3
                   textcoor33 textcoor1
                   textcoor44 textcoor2
             )
           )
           (if (> (car textcoorbmm) (car textcoor1)) 
             (setq textcoor11 textcoor3
                   textcoor22 textcoor4
                   textcoor33 textcoor2
                   textcoor44 textcoor1
             )
             (setq textcoor11 textcoor3
                   textcoor22 textcoor4
                   textcoor33 textcoor1
                   textcoor44 textcoor2
             )
           )
         )
        ) ;cond if
        ((= textcoortm textcoorbmm)
         (if (> (car textcoorbmm) (car textcoor2)) 
           (if (> (car textcoortmm) (car textcoor3)) 
             (setq textcoor11 textcoor2
                   textcoor22 textcoor1
                   textcoor33 textcoor4
                   textcoor44 textcoor3
             )
             (setq textcoor11 textcoor2
                   textcoor22 textcoor1
                   textcoor33 textcoor3
                   textcoor44 textcoor4
             )
           )
           (if (> (car textcoortmm) (car textcoor3)) 
             (setq textcoor11 textcoor1
                   textcoor22 textcoor2
                   textcoor33 textcoor4
                   textcoor44 textcoor3
             )
             (setq textcoor11 textcoor1
                   textcoor22 textcoor2
                   textcoor33 textcoor3
                   textcoor44 textcoor4
             )
           )
         )
        ) ;cond if
        ((= textcoortm textcoorlmm)
         (if (> (car textcoorlmm) (car textcoor1)) 
           (if (> (car textcoorrmm) (car textcoor2)) 
             (setq textcoor11 textcoor1
                   textcoor22 textcoor4
                   textcoor33 textcoor3
                   textcoor44 textcoor2
             )
             (setq textcoor11 textcoor1
                   textcoor22 textcoor4
                   textcoor33 textcoor2
                   textcoor44 textcoor3
             )
           )
           (if (> (car textcoorrmm) (car textcoor2)) 
             (setq textcoor11 textcoor4
                   textcoor22 textcoor1
                   textcoor33 textcoor3
                   textcoor44 textcoor2
             )
             (setq textcoor11 textcoor4
                   textcoor22 textcoor1
                   textcoor33 textcoor2
                   textcoor44 textcoor3
             )
           )
         )
        ) ;cond if
        ((= textcoortm textcoorrmm)
         (if (> (car textcoorrmm) (car textcoor3)) 
           (if (> (car textcoorlmm) (car textcoor4)) 
             (setq textcoor11 textcoor3
                   textcoor22 textcoor2
                   textcoor33 textcoor1
                   textcoor44 textcoor4
             )
             (setq textcoor11 textcoor3
                   textcoor22 textcoor2
                   textcoor33 textcoor4
                   textcoor44 textcoor1
             )
           )
           (if (> (car textcoorlmm) (car textcoor4)) 
             (setq textcoor11 textcoor2
                   textcoor22 textcoor3
                   textcoor33 textcoor1
                   textcoor44 textcoor4
             )
             (setq textcoor11 textcoor2
                   textcoor22 textcoor3
                   textcoor33 textcoor4
                   textcoor44 textcoor1
             )
           )
         )
        ) ;cond if
      )
      ;;邊框交點數字
      (setq textcoorrm (list (/ (+ (car textcoor22) (car textcoor33)) 2) 
                             (/ (+ (cadr textcoor22) (cadr textcoor33)) 2)
                       )
      )
      (setq textcoorlm (list (/ (+ (car textcoor11) (car textcoor44)) 2) 
                             (/ (+ (cadr textcoor11) (cadr textcoor44)) 2)
                       )
      )
      (setq tnum 0
            bnum 0
            rnum 0
            lnum 0
      )
      (repeat allintnum 
        (setq ptcr (list)) ;;單一交點座標
        (repeat 3 
          (setq ptcr (cons (car intcoor) ptcr))
          (setq intcoor (cdr intcoor))
        ) ;repeat
        (setq ptcr (reverse (cdr ptcr)))
        (setq aaaa (- (car ptcr) (car textcoor44)))
        (setq aaab (rtos (- (car textcoor11) (car textcoor44)) 2 4))
        (cond 
          ((= (+ (- (car ptcr) (car textcoor11)) (- (cadr ptcr) (cadr textcoor11))) 
              0
           )
          ) ;;如交點切到坵塊角不計算
          ((= (+ (- (car ptcr) (car textcoor22)) (- (cadr ptcr) (cadr textcoor22))) 
              0
           )
          )
          ((= (+ (- (car ptcr) (car textcoor33)) (- (cadr ptcr) (cadr textcoor33))) 
              0
           )
          )
          ((= (+ (- (car ptcr) (car textcoor44)) (- (cadr ptcr) (cadr textcoor44))) 
              0
           )
          )
          ;;若左右兩邊為垂直
          ((= (rtos (- (car ptcr) (car textcoor44)) 2 4) 
              (rtos (- (car textcoor11) (car textcoor44)) 2 4)
           )
           (setq lnum (+ lnum 1))
          ) ;左邊
          ((= (rtos (- (car ptcr) (car textcoor33)) 2 4) 
              (rtos (- (car textcoor22) (car textcoor33)) 2 4)
           )
           (setq rnum (+ rnum 1))
          ) ;右邊
          ;;計算斜率
          ((= 
             (rtos 
               (/ 
                 (- (cadr textcoor22) (cadr textcoor11))
                 (- (car textcoor22) (car textcoor11))
               )
               2
               4
             )
             (rtos 
               (/ 
                 (- (cadr ptcr) (cadr textcoor11))
                 (- (car ptcr) (car textcoor11))
               )
               2
               4
             )
           ) ;if
           (setq tnum (+ tnum 1))
          ) ;;1
          ((= 
             (rtos 
               (/ 
                 (- (cadr textcoor33) (cadr textcoor44))
                 (- (car textcoor33) (car textcoor44))
               )
               2
               4
             )
             (rtos 
               (/ 
                 (- (cadr ptcr) (cadr textcoor44))
                 (- (car ptcr) (car textcoor44))
               )
               2
               4
             )
           ) ;if
           (setq bnum (+ bnum 1))
          ) ;;2
          ((= 
             (rtos 
               (/ 
                 (- (cadr textcoor11) (cadr textcoor44))
                 (- (car textcoor11) (car textcoor44))
               )
               2
               4
             )
             (rtos 
               (/ 
                 (- (cadr ptcr) (cadr textcoor44))
                 (- (car ptcr) (car textcoor44))
               )
               2
               4
             )
           ) ;if
           (setq lnum (+ lnum 1))
          ) ;;3
          ((= 
             (rtos 
               (/ 
                 (- (cadr textcoor22) (cadr textcoor33))
                 (- (car textcoor22) (car textcoor33))
               )
               2
               4
             )
             (rtos 
               (/ 
                 (- (cadr ptcr) (cadr textcoor33))
                 (- (car ptcr) (car textcoor33))
               )
               2
               4
             )
           ) ;if
           (setq rnum (+ rnum 1))
          ) ;;4
        ) ;cond
        (setq ptcr nil)
      ) ;repeat
      (command "-text" 
               "j"
               "m"
               textcoortm
               (* texth scale)
               (/ (* rectrad 180) pi)
               (itoa tnum)
      )
      (command "-text" 
               "j"
               "m"
               textcoorbm
               (* texth scale)
               (/ (* rectrad 180) pi)
               (itoa bnum)
      )
      (command "-text" 
               "j"
               "m"
               textcoorlm
               (* texth scale)
               (/ (* rectrad 180) pi)
               (itoa lnum)
      )
      (command "-text" 
               "j"
               "m"
               textcoorrm
               (* texth scale)
               (/ (* rectrad 180) pi)
               (itoa rnum)
      )
      ;;-----歸0
      (setq line0 (+ line0 1))
      (setq intcoor nil)
      ;;-----將方型1 2角位做串列
      (setq rectang34 (cons 
                        (list 
                          (list (atof (rtos (car textcoor11) 2 3)) 
                                (atof (rtos (cadr textcoor11) 2 3))
                          )
                          (list (atof (rtos (car textcoor22) 2 3)) 
                                (atof (rtos (cadr textcoor22) 2 3))
                          )
                        )
                        rectang34
                      )
      ) ;rtos控制精度
    ) ;repeat rectnum
    ;;----左上角編號文字---

    ;;新增編號文字圖層
    (setq ly (tblsearch "layer" "坡度分析編號"))
    (if (= ly nil) 
      (command ".-layer" "n" "坡度分析編號" "c" "30" "坡度分析編號" "")
    )
    (setvar "clayer" "坡度分析編號")

    ;;判斷幾行 column
    (setq column     0
          col        0
          rectang34a rectang34
          rectang34b rectang34
          rectf      (list)
          rectfre    (list)
    )

    (repeat rectnum 
      (setq compare (car (car rectang34a)))
      (setq rectang34a (cdr rectang34a))
      (setq rectang34b (subst '((0 0) (0 0)) compare rectang34b))
      (repeat rectnum 
        (if (= (car compare) (car (cadr (car rectang34b)))) 
          (if (= (cadr compare) (cadr (cadr (car rectang34b)))) 
            (setq col (+ col 1))
            (setq col (+ col 0))
          ) ;if
        ) ;if
        (setq rectang34b (cdr rectang34b))
      ) ;repeat2
      (if (= col 0) 
        (progn 
          (setq column  (+ column 1)
                rectf   (cons compare rectf)
                rectfre (cons (reverse compare) rectfre)
          )
        )
        (setq column (+ column 0))
      )
      (setq col 0)
      (setq rectang34b rectang34)
    ) ;repeat1
    ;找第一排方塊
    (setq rectang34a rectfre
          rectang34b (list)
          eng        65
          fnum       1
          ceng       1
    )
    ;;找Y最大值
    (repeat column 
      (setq rectang34b (cons (car (car rectang34a)) rectang34b))
      (setq rectang34a (cdr rectang34a))
    )
    ;;由上而下輸入編號
    (repeat column 
      (setq bigy (apply 'max rectang34b))
      (setq miny (apply 'min rectang34b))
      (setq firstcoor (reverse (assoc bigy rectfre)))
      (setq ficolist (assoc firstcoor rectang34))
      (if (> eng 90) 
        (setq eng  (- eng 26)
              ceng (+ ceng 1)
        )
      ) ;end if
      ;若超過26英文字母(未完成,ceng=超過幾次26)
      (command "-text" 
               "j"
               "tl"
               firstcoor
               (* texth scale)
               (/ (* rectrad 180) pi)
               (strcat (chr eng) (itoa fnum))
      )

      ;(command "-text" "j" "tl" firstcoor (* texth scale) (/ (* rectrad 180) pi) (strcat (chr eng) (itoa fnum)))

      (setq out 0)
      ;;由左而右輸入編號
      (setq firstcoor (cadr ficolist))
      (while (= out 0) 
        (setq firstcoor (assoc firstcoor rectang34))
        (if (/= firstcoor nil) 
          (progn 
            (setq fnum (+ fnum 1))
            (command "-text" 
                     "j"
                     "tl"
                     (car firstcoor)
                     (* texth scale)
                     (/ (* rectrad 180) pi)
                     (strcat (chr eng) (itoa fnum))
            )
            (setq rectang34 (subst '(0 0 0 0) firstcoor rectang34)
                  firstcoor (cadr firstcoor)
            )
          ) ;progn
          (setq out 1)
        ) ;if
      ) ;while
      ;;將已輸入的Y值刪除
      (setq rectang34b (subst (- miny 1) bigy rectang34b))
      (setq eng  (+ eng 1)
            fnum 1
      )
    ) ;由上而下輸入編號end
    ;;左上角編號文字end
    ;;---------------------------填入文字end
    ;;----------------------------
  ) ;;defun
  ;;------------------填入文字end---------------------
  ;-------------------坵塊資料------------------------
  (defun sloperectdata () 

    (setq rectsda (entget rectsel))
    (setq rectlayer (assoc 8 rectsda))
    ;;若為block則炸開
    (if (= (cdr (assoc 0 (entget rectsel))) "INSERT") 
      (command "_explode" (ssget "x" (list '(0 . "INSERT") rectlayer)))
    )
    ;;boom end
    (setq allrect (ssget "x" (list '(0 . "LWPOLYLINE") rectlayer)))
    (setq rectnum (sslength allrect)) ;;邱塊格數
  )
  ;-------------------坵塊資料end---------------------
  ;;-------------------**副程式end**-------------------------------------;;
  ;;*********************************************************************;;
  ;;*********************************************************************;;
  ;;*********************************************************************;;
  ;;*********************************************************************;;
  ;;-------------------**主程式**----------------------------------------;;
  (setq gw 25) ;;global width預設坵塊邊長
  (setq texth 3) ;;文字高度
  (setq deltah 1)

  ;;------求等高線間距(預設1)
  (setq deltah (getint "請輸入等高線間距"))
  (terpri)
  ;;------求坵塊方塊資料
  (setq rectsel (car (entsel "選取分析坵塊")))
  (terpri)

  ;;------求等高線資料
  (setq line1msel (car (entsel "選取等高線1")))
  (setq line5msel (car (entsel "選取等高線2")))
  ;;------求坵塊資料
  (sloperectdata)
  (setq line1mlayer (assoc 8 (entget line1msel)))
  (setq line5mlayer (assoc 8 (entget line5msel)))
  ;(prompt "\nSelect Objects by Window")
  ;(setq p1 (getpoint "\nPick First Point"))
  ;(setq p2 (getcorner p1 "\nPick Second Point"))
  ;(setq sel1 (ssget "_w" p1 p2 '((0 . "insert"))))
  ;(setq linesel (ssget "W" "\n窗選等高線圖層: "))
  ;;------求地界線資料
  ;(setq limitsel (car (entsel "選取地界")))
  ;(if (= limitsel nil)
  ;(setq limitsel nil)
  ;(setq limitsel nil)
  ;)
  ;;------填入文字
  (command "-style" "slope" "times new roman" "0" "" "" "" "") ;;改字型
  (inserttext)
  ;;將各坵塊轉成圖塊
  (setvar "clayer" (cdr rectlayer))
  (command "-purge" "b" "坵塊分析" "y" "y")
  (command "-block" "坵塊分析" textcoor1 allrect "")
  (command "-insert" "坵塊分析" textcoor1 "1" "1" "0")


  ;;------------------------------------------------end--------------------------------
  (setvar "osmode" 16383) ;;恢復鎖點
  (princ)
);;end-----------------------------------