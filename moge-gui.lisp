(load "define.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)
(load "util.lisp" :external-format :utf-8)
(load "make-monster.lisp" :external-format :utf-8)

;;json用player情報リスト作成
(defun player-list (p)
  (list :|player|
	 (list :|hp|        (player-hp p)    :|maxhp|  (player-maxhp p)
	       :|str|       (player-str p)   :|maxstr| (player-maxstr p)
	       :|agi|       (player-agi p)   :|maxagi| (player-maxagi p)
	       :|level|     (player-level p) :|exp|    (player-exp p)
	       :|heal|      (player-heal p)  :|hammer| (player-hammer p)
	       :|map-level| (player-map p)
	       :|buki|      (player-buki p)
	       :|pos|   (list :|x| (player-posx p) :|y| (player-posy p)))))

(defun ai-kill-proc (ai)
  (let ((unix-signal-term 15))
    (when (ai-proc ai)
      (sb-ext:process-kill (ai-proc ai) unix-signal-term))))

(defun kill-all-ais ()
  (loop for ai in *ais*
	do
	(ai-kill-proc ai)))

(defun init-data ()
  (setf *battle?* nil
	*monster-num* 6
	*monster-level* 1
	*boss?* 0
	*end* 0
	*lv-exp* 100
	*start-time* (get-internal-real-time)
	*ha2ne2* nil
	*copy-buki* (copy-tree *buki-d*)))

(defun get-ai-command-line ()
  (with-open-file (in "ai.txt" :direction :input)
    (format nil "~a" (read-line in nil))))

(define-condition handshake-error (error) ())

(defun ascii->zenkaku (char)
  (code-char (+ 65248 (char-code char))))

(defun atama-of (name)
  (let ((atama (char name 0)))
    (cond
     ((= 2 (moge-char-width atama))
      (format nil "~c" atama))
     ((= 1 (moge-char-width atama))
      (format nil "~c" (ascii->zenkaku atama))))))

(defstruct ai
  command-line
  proc
  stream
  atama
  name)

;; AIから名前を受け取る。
(defun receive-name (ai)
  (handler-case
   (let ((name (read-line (ai-stream ai))))
     (when (equal name "")
	   (format t "AIの名前が空です。~%")
	   ;;(setf mozi (concatenate 'string mozi (format nil "AIの名前が空です。~%")))
	   (error 'handshake-error))
     (setf (ai-name ai) name
	   (ai-atama ai) (atama-of name)))
   (end-of-file (c)
		(format t "~A~%" c)
		;;(setf mozi (concatenate 'string mozi (format nil "~A~%" c)))
		(error 'handshake-error))))

(defun load-ai2 (com)
  (destructuring-bind
      (command-name . args)
      (ppcre:split #\space com)
    (let* ((proc (sb-ext:run-program
		  command-name args
		  :input :stream
		  :output :stream
		  :wait nil
		  :search t))
	   (stream (make-two-way-stream
		    (sb-ext:process-output proc)
		    (sb-ext:process-input proc)))
	   (ai (make-ai :command-line com
			:proc proc
			:stream stream)))
      (receive-name ai)
      ai)))

;;ゲームオーバーメッセージ
(defun game-over-message (p gamen)
  (let ((hoge nil))
    (format t "Game Over.~%")
    (setf hoge "Game Over.~%")
    (format t "あなたは地下~d階で力尽きた。~%" (player-map p))
    (setf hoge (concatenate 'string hoge (format nil "あなたは地下~d階で力尽きた。~%" (player-map p))))
    (setf (text gamen) (format nil hoge))))


;;武器装備してステータス更新
(defun equip-buki (item p)
  (incf (player-hp p)     (- (third item) (third (player-buki p))))
  (incf (player-maxhp p)  (- (third item) (third (player-buki p))))
  (incf (player-str p)    (- (second item) (second (player-buki p))))
  (incf (player-maxstr p) (- (second item) (second (player-buki p))))
  (incf (player-agi p)    (- (fourth item) (fourth (player-buki p))))
  (incf (player-maxagi p) (- (fourth item) (fourth (player-buki p))))
  (setf (player-buki p) item))

;;装備してる武器と見つけた武器のリスト
(defun equip-list (p item)
  (let ((now-buki (player-buki p)))
    (append (list :|equip| 1)
	    (list :|now|
		  (list :|name| (first now-buki) :|str| (second now-buki)
			:|hp| (third now-buki) :|agi| (fourth now-buki)))
	    (list :|discover|
		  (list :|name| (first item) :|str| (second item)
			:|hp| (third item) :|agi| (fourth item))))))

;;装備モード入出力
(defun equip-select (p item)
  (let ((str nil)
	(ai-stream (ai-stream (nth (player-num p) *ais*))))
    (format ai-stream "~a~%" (jonathan:to-json (equip-list p item)))
    (finish-output ai-stream)
    (setf str (read-line ai-stream))
    (cond
      ((string= str "YES")
       (format t "「~aを装備した。」~%" (first item))
       (equip-buki item p))
      (t nil))))

;;見つけた武器を装備するか
(defun equip? (p item)
  (format t "「~aを見つけた」~%" (first item))
  (format t "現在の装備品：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	      (first (player-buki p)) (second (player-buki p))
	      (third (player-buki p)) (fourth (player-buki p)))
  (format t "発見した装備：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	      (first item) (second item) (third item) (fourth item))
  (format t "「装備しますか？」(z:装備 x:捨てる c:袋にしまう)~%")
  (equip-select p item))

;;レベルアップポイント振り分け入出力
(defun point-wake (p n)
  (if (= n 0)
      ;;振り分け終わったらステータス全回復
      (setf (player-hp p) (player-maxhp p)
	    (player-str p) (player-maxstr p)
	    (player-agi p) (player-maxagi p))
      (let ((str nil))
	(format *ai* "~a~%" (jonathan:to-json (append (list :|levelup| 1)(player-list p))))
	(finish-output *ai*)
	(setf str (read-line *ai*))
	(cond
	  ((string= str "HP")
	   (incf (player-maxhp p))
	   (point-wake p (1- n)))
	  ((string= str "STR")
	   (incf (player-maxstr p))
	   (point-wake p (1- n)))
	  ((string= str "AGI")
	   (incf (player-maxagi p))
	   (point-wake p (1- n)))
	  (t nil)))))
;;戦闘終了後レベルアップ
(defun level-up (p)
  (loop while (>= (player-exp p) *lv-exp*) do
    (let ((point (randval 3)))
      (point-wake p point)
      (decf (player-exp p) *lv-exp*)
      (incf (player-level p))
      (incf *lv-exp* 10))))
;;戦闘終了後アイテム入手
(defun item-drop? (p)
  ;;(gamen-clear)
  (dolist (item (player-drop p))
    (let ((buki (assoc item *event-buki* :test #'equal)))
      (cond
	(buki (equip? p buki))
	((string= item "ハンマー")
	 (format t "「ハンマーを拾った！」~%")
	 (incf (player-hammer p)))
	((string= item "回復薬")
	 (format t "「回復薬を拾った！」~%")
	 (incf (player-heal p))))
      (setf (player-drop p) nil)))) ;;ドロップ品を消す

;;ランダムなモンスターグループを作る
(defun init-monsters (p)
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       ;;(funcall (nth (random (length *monster-builders*)) *monster-builders*)))
               (let ((y (random 101)))
		 ;;モンスターの出現率
                 (cond
                   ((<= 0 y 25) (make-orc))
                   ((<= 26 y 50) (make-hydra))
                   ((<= 51 y 75) (make-slime-mold))
                   ((<= 76 y 99) (make-brigand))
                   (t (make-yote1 :health 3)))))
	     (make-array (setf (player-monster-num p)
			       (randval (+ *monster-num* (floor (player-level p) 4))))))))
;;配列の０番目にボス、あとはランダムなモンスター(m=0,もげぞう m=1,ハツネツ)
(defun boss-monsters (p m)
  (let ((hoge 0))
    (setf *battle-delay-seconds* *boss-delay-seconds*)
    (setf *monsters*
	  (map 'vector
	       (lambda (x)
		 (if (= hoge 0)
		     (progn (incf hoge)
			    (cond
			      ((= m 0) (make-boss :health 300))
			      ((= m 1) (make-ha2ne2 :health 220))))
		     (funcall (nth (random (length *monster-builders*))
				   *monster-builders*))))
	       (make-array 10)))
    (setf (player-monster-num p) 10)))


;;プレイヤーの生死判定
(defun player-dead (p)
  (<= (player-hp p) 0))

(defun show-player2 (p name)
  (format nil "Lv ~d, ~a : HP ~d/~d 力 ~d/~d 素早さ ~d/~d ~%"
	      (player-level p) name (player-hp p) (player-maxhp p)  (player-str p) (player-maxstr p)
	      (player-agi p) (player-maxagi p)))

;;モンスターの生死判定
(defun monster-dead (m)
  (<= (monster-health m) 0))
;;モンスターグループが全滅したか判定
(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;;ステータスとバトルコマンド表示
(defun status-and-command (p)
  (let ((hoge nil))
    (format t "------------------------------------------------------------~%")
    (format t ":ステータス~%")
    (setf hoge "------------------------------------------------------------~%:ステータス~%")
    (loop for i from 0 to 5
	  do
	     (case i
	       (0 (format t "L v  ~2d~%" (player-level p))
		(setf hoge (concatenate 'string hoge (format nil "L v  ~2d~%" (player-level p)))))
	       (1 (format t "H P  ~2d/~2d~%" (player-hp p) (player-maxhp p))
		(setf hoge (concatenate 'string hoge (format nil "H P  ~2d/~2d~%" (player-hp p) (player-maxhp p)))))
	       (2 (format t "ATK  ~2d/~2d~%" (player-str p) (player-maxstr p))
		(setf hoge (concatenate 'string hoge (format nil "ATK  ~2d/~2d~%" (player-str p) (player-maxstr p)))))
	       (3 (format t "AGI  ~2d/~2d~%" (player-agi p) (player-maxagi p))
		(setf hoge (concatenate 'string hoge (format nil "AGI  ~2d/~2d~%" (player-agi p) (player-maxagi p)))))
	       (4 (format t "HEAL ~2d~%"     (player-heal p))
		(setf hoge (concatenate 'string hoge (format nil "HEAL ~2d~%"     (player-heal p)))))
	       (5 (format t "EXP ~3d/~3d~%" (player-exp p) *lv-exp*)
		(setf hoge (concatenate 'string hoge (format nil "EXP ~3d/~3d~%" (player-exp p) *lv-exp*))))))
    hoge))

;;-----------------敵からのアイテムドロップ-------------------------
(defun yote1-drop (p)
  (if (= 1 (random 100))
      (push "メタルヨテイチの剣" (player-drop p))))
(defun ha2ne2-drop (p)
  (if (= 0 (random 1)) ;;とりあえず100%
      (push "ハツネツの剣" (player-drop p))))

(defun orc-drop (p)
  (if (= 1 (random 20))
      (push "ハンマー" (player-drop p))))
(defun slime-drop (p)
  (if (= 1 (random 20))
      (push "回復薬" (player-drop p))))
;;-----------------------------------------------------------------
;;モンスターの受けたダメージ処理
(defmethod monster-hit2 (p m x)
  (decf (monster-health m) x)
  (incf (monster-damage m) x)
  ;;倒したら経験値取得
  (if (monster-dead m)
      (case (type-of m)
        (ha2ne2
	 (ha2ne2-drop p)
	 (incf (player-exp p) 99))
	(orc
	 (orc-drop p)
	 (incf (player-exp p) 2))
	(slime-mold
	 (slime-drop p)
	 (incf (player-exp p) 3))
	(hydra
	 (incf (player-exp p) 4))
	(brigand
	 (incf (player-exp p) 5)))))
;;------メタルヨテイチ-------------
(defmethod monster-hit2 ((p player) (m yote1) x)
  (decf (monster-health m))
  (incf (monster-damage m))
  (if (monster-dead m)
      (progn (incf (player-exp p) 100)
	     (yote1-drop p))))
;;-------------------------------------------
;;薬を使う
(defun use-heal (p)
  (cond
    ((>= (player-heal p) 1)
     (format t "「回復薬を使った。」~%")
     (decf (player-heal p))
     (setf (player-hp p)  (player-maxhp p)
	   (player-agi p) (player-maxagi p)
	   (player-str p) (player-maxstr p))
     (format nil "「回復薬を使った。」~%"))
    (t
     (format t "「回復薬を持っていません！」~%")
     (format nil "「回復薬を持っていません！」~%"))))

;;ランダムでモンスターを選択
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

;;(カーソル付きで)敵表示
(defun show-pick-monsters ()
  (let ((hoge nil))
    (fresh-line)
    (format t "-----------------------敵が現れた！-------------------------~%")
    (format t "敵:~%")
    (setf hoge "-----------------------敵が現れた！-------------------------~%敵:~%")
    (loop for m across *monsters*
	  for x = 0 then x
	  do
	     (cond
	       ((monster-dead m)
		(format t "~a" (minimum-column 3 ""))
		(setf hoge (concatenate 'string hoge (format nil "~a" (minimum-column 3 ""))))
		(format t "~c."  (number->a x))
		(setf hoge (concatenate 'string hoge (format nil "~c."  (number->a x))))
		(incf x)
		(if (> (monster-damage m) 0)
		    (progn
		      (format t "~a" (minimum-column 31 "**死亡**"))
		      (setf hoge (concatenate 'string hoge (format nil "~a" (minimum-column 31 "**死亡**"))))
		      (format t "~d のダメージを与え倒した！~%" (monster-damage m))
		      (setf hoge (concatenate 'string hoge
					      (format nil "~d のダメージを与え倒した！~%" (monster-damage m)))))
		    (progn
		      (format t "**死亡**~%")
		      (setf hoge (concatenate 'string hoge (format nil "**死亡**~%"))))))
	       (t
		(format t "~a" (minimum-column 3 ""))
		(setf hoge (concatenate 'string hoge (format nil "~a" (minimum-column 3 ""))))
		(format t "~c."  (number->a x))
		(setf hoge (concatenate 'string hoge (format nil "~c."  (number->a x))))
		(incf x)
		(format t "~a"
			(minimum-column 9 (format nil "(HP=~d) " (monster-health m))))
		(setf hoge (concatenate 'string hoge
					(format nil "~a"
						(minimum-column 9 (format nil "(HP=~d) " (monster-health m))))))
		(format t "~a" (minimum-column 22 (monster-show m)))
		(setf hoge (concatenate 'string hoge
					(format nil "~a" (minimum-column 22 (monster-show m)))))
		(if (> (monster-damage m) 0)
		    (progn
		      (format t "~d のダメージを与えた！~%" (monster-damage m))
		      (setf hoge (concatenate 'string hoge
					      (format nil "~d のダメージを与えた！~%" (monster-damage m)))))
		    (progn
		      (fresh-line)
		      (setf hoge (concatenate 'string hoge "~%"))))))
	     (setf (monster-damage m) 0)) ;;与えたダメージリセット
    hoge))

;;モンスター配列をリスト化
(defun monster-list ()
  (let ((lst nil))
    (loop for m across *monsters*
	  for i from 0 do
      (case (type-of m)
	(orc (push (list :|name| "オーク" :|number| i :|level| (orc-club-level m)
					  :|hp| (monster-health m)) lst))
	(hydra (push (list :|name| "ヒドラ" :|number| i :|level| (monster-health m)
					    :|hp| (monster-health m)) lst))
	(slime-mold (push (list :|name| "スライム" :|number| i
				:|level| (slime-mold-sliminess m)
						   :|hp| (monster-health m)) lst))
	(brigand (push (list :|name| "ブリガンド" :|number| i :|level| (brigand-atk m)
						  :|hp| (monster-health m)) lst))
	(yote1 (push (list :|name| "メタルヨテイチ" :|number| i :|level| (yote1-atk m)
						    :|hp| (monster-health m)) lst))
	(ha2ne2 (push (list :|name| "ハツネツエリア" :|number| i :|level| 50
						     :|hp| (monster-health m)) lst))
	(boss (push (list :|name| "もげぞう" :|number| i :|level| 100
			  :|hp| (monster-health m)) lst))))
    (list :|monsters| lst)))

;;モンスター配列をリスト化(モンスターの攻撃付き)
(defun monster-attack-list (p)
  (let ((lst nil))
    (loop for m across *monsters*
	  for i from 0 do
	    (if (not (monster-dead m))
		(case (type-of m)
		  (orc (push (list :|name| "オーク" :|number| i :|level| (orc-club-level m)
						    :|hp| (monster-health m)
						    :|damage| (monster-attack m p)) lst))
		  (hydra (push (list :|name| "ヒドラ" :|number| i :|level| (monster-health m)
						      :|hp| (monster-health m)
						      :|damage| (monster-attack m p)) lst))
		  (slime-mold (push (list :|name| "スライム" :|number| i
					  :|level| (slime-mold-sliminess m)
							     :|hp| (monster-health m)
							     :|damage| (monster-attack m p)) lst))
		  (brigand (push (list :|name| "ブリガンド" :|number| i :|level| (brigand-atk m)
							    :|hp| (monster-health m)
							    :|damage| (monster-attack m p)) lst))
		  (yote1 (push (list :|name| "メタルヨテイチ" :|number| i :|level| (yote1-atk m)
							      :|hp| (monster-health m)
							      :|damage| (monster-attack m p)) lst))
		  (ha2ne2 (push (list :|name| "ハツネツエリア" :|number| i :|level| 50
							       :|hp| (monster-health m)
							       :|damage| (monster-attack m p)) lst))
		  (boss (push (list :|name| "もげぞう" :|number| i :|level| 100
						       :|hp| (monster-health m)
						       :|damage| (monster-attack m p)) lst)))))
    (list :|monsters| lst)))

;;攻撃方法入出力
(defun player-attack2 (p gamen)
  (let ((str-l nil) (str nil) (act nil) (hoge nil))
    (format *ai* "~a~%" (jonathan:to-json (append (list :|battle| 1) (monster-list) (player-list p))))
    (finish-output *ai*)
    (setf str-l (read-line *ai*)
	  str (ppcre:split #\space str-l)
	  act (car str))
    (cond
      ((find act '("HEAL" "SWING" "STAB" "DOUBLE") :test #'equal)
       ;;(gamen-clear)
       (setf hoge (show-pick-monsters))
       (setf hoge (concatenate 'string hoge (status-and-command p)))

       (cond
	 ((string= act "HEAL")
	  (setf hoge (concatenate 'string hoge (use-heal p))))
	 ((string= act "SWING")
	  (format t "「なぎはらい！」~%")
	  (setf  hoge  (concatenate 'string hoge (format nil "「なぎはらい！」~%")))
	  (dotimes (x (1+ (randval (truncate (/ (player-str p) 3)))))
	    (unless (monsters-dead)
	      (monster-hit2 p (random-monster) 1))))
	 ((string= act "STAB")
	  (format t "「~c に斬りかかった！」~%" (number->a (parse-integer (cadr str))))
	  (setf hoge (concatenate 'string hoge
				  (format nil "「~c に斬りかかった！」~%" (number->a (parse-integer (cadr str))))))
	  (let ((m (aref *monsters* (parse-integer (cadr str)))))
	    (monster-hit2 p m (+ 2 (randval (ash (player-str p) -1))))))
	 ((string= act "DOUBLE")
	  (format t "「~c と ~c にダブルアタック！」~%" (number->a (parse-integer (second str)))
		  (number->a (parse-integer (third str))))
	  (setf hoge (concatenate 'string hoge
				  (format nil "「~c と ~c にダブルアタック！」~%"
					  (number->a (parse-integer (second str)))
					  (number->a (parse-integer (third str))))))
	  (let ((m (aref *monsters* (parse-integer (second str))))
		(x (randval (truncate (/ (player-str p) 6)))))
	    (monster-hit2 p m x) ;;選ばれたモンスターにダメージ与える
	    (unless (monsters-dead) ;;生き残ってるモンスターがいるなら２回目の攻撃
	      (let ((m2 (aref *monsters* (parse-integer (third str)))))
		(if (monster-dead m2)
		    (monster-hit2 p (random-monster) x)
		  (monster-hit2 p m2 x)))))))
       (sleep *battle-delay-seconds*))
      (t (format t "~A~%" str-l) ;;規定文字列以外の表示(エラーとか)
	 (setf *end* 2)))
    (setf (text gamen) (format nil hoge))))

;;勝利メッセージ
(defun victory-message ()
  (format t "~%")
  (format t "「大 勝 利 ！」~%"))

;;バトル時、プレイヤーが死ぬかモンスターが全滅するまでループ
(defun game-loop (p gamen)
  (unless (or (player-dead p) (monsters-dead) *battle-end*)
    (dotimes (k (1+ (truncate (/ (max 0 (player-agi p)) 15))))
      (unless (or (monsters-dead) (= *end* 2) *battle-end*)
	(player-attack2 p gamen)
	(ltk:process-events)))
    (cond
      ((or (= *end* 2) *battle-end*) ;;エラー集雨量
       nil)
      ((null (monsters-dead))
       ;;敵の攻撃ダメージ情報を渡すだけ
       (format *ai* "~a~%" (jonathan:to-json (append (list :|damage-info| 1) (monster-attack-list p)
						     (player-list p))))
       (finish-output *ai*)
       ;;(monster-attack-list p)
       (ltk:process-events)
       (game-loop p gamen)))))

(defun monsters-seisei (p)
  (cond ;;モンスターズ作成
    ((= *boss?* 1) ;;ラスボス
     (boss-monsters p 0))
    ((= *boss?* 2) ;;中ボス
     (boss-monsters p 1))
    ((= *boss?* 0) ;;雑魚
     (init-monsters p)))
  (setf *battle-state* 1))

;;バトル開始
(defun orc-battle (p gamen)
  (cond ;;モンスターズ作成
    ((= *boss?* 1) ;;ラスボス
     (boss-monsters p 0))
    ((= *boss?* 2) ;;中ボス
     (boss-monsters p 1))
    ((= *boss?* 0) ;;雑魚
     (init-monsters p)))
  (game-loop p gamen) ;;バトルループ
  ;;(if (= *battle-state* 2)
  (cond
    ((player-dead p) ;;プレイヤーが死んだとき
     (game-over-message p gamen)
     (setf *end* 2))
    ((or (= *end* 2) *battle-end*) ;;エラー終了
     nil)
    (t ;;(monsters-dead) 敵を倒したとき
     ;;(gamen-clear)
     (level-up p) ;;レベルアップ処理
     (if (player-drop p)
	 (item-drop? p)) ;;アイテム入手処理
     (cond
       ((= *boss?* 1)
	(setf *end* 1) ;;ラスボスならエンディングへ
	(setf *battle-delay-seconds* *bds*) ;;バトルディレイを元に戻す
	(victory-message))
       ((= *boss?* 2)
	(setf *ha2ne2* t) ;;中ボス倒したフラグ
	(setf *battle-delay-seconds* *bds*))) ;;バトルディレイを元に戻す
     ;;バトルフラグとボスフラグを初期化
     (setf *battle?* nil
	   *boss?* 0))))


;;-----------------------マップ------------------------------------------------------------
;;---------------------------------------------------------------------------------------
;;マップ移動
(defun show-msg (p)
  (if (player-msg p)
      (format t "~a~%" (player-msg p)))
  (setf (player-msg p) nil))

(defun map-char (num)
  (case num
    (30 "□") ;; 壁
    (40 "■") ;; 壊せない壁
    (0  "　")
    (4  "薬") ;; 薬
    (5  "ボ") ;;ボス
    (3  "宝") ;; 宝箱
    (2  "下") ;; 下り階段
    (6  "イ") ;; イベント
    (7  "ハ") ;; 中ボス ハツネツエリア
    (otherwise
     (if (<= 10 num 13) ;; 10~13: AI1 AI2 AI3 AI4
	 (ai-atama (nth (- num 10) *ais*))
       (error (format nil "不明なセル番号: ~s" num))))))

(defun write-player-stat (stream p name)
  (format stream "~a" (show-player2 p name))
  (destructuring-bind
      (name str hp agi)
      (player-buki p)
    (format stream "武器 ~a" name)
    (format stream " (HP:~d STR:~d AGI:~d)" hp str agi))
  (format stream " 回復薬 ~d個" (player-heal p))
  (format stream " ハンマー ~d個" (player-hammer p))
  (format stream " Exp ~d" (player-exp p))
  (fresh-line stream))

(defun write-player-message (stream p)
  (when (player-msg p)
    (format stream "~a~a~%" (ai-name (nth (player-num p) *ais*)) (player-msg p))
    (setf (player-msg p) nil)))

(defun show-map2 (map players)
  (let ((out (make-string-output-stream)))
    (format out "地下~d階~%" (player-map (first players)))
    (loop for p in players
	  for ai in *ais*
	  do
	  (write-player-stat out p (ai-name ai)))
    (fresh-line out)
    (loop for i from 0 below (donjon-tate map) do
      (loop for j from 0 below (donjon-yoko map) do
	    (format out "~a"
		    (map-char (aref (donjon-map map) i j))))
      (fresh-line out))
    (loop for p in players
	  for i from 0
	  do
	  (write-player-message out p))
    (get-output-stream-string out)))

(defun map-data-list (map)
  (let ((blocks nil)
	(walls nil)
	(items nil)
	(boss nil)
	(kaidan nil)
	(ha2 nil)
	(events nil))
    (loop for i from 0 below (donjon-tate map) do
      (loop for j from 0 below (donjon-yoko map) do
	(let ((x (aref (donjon-map map) i j)))
	  (case x
	    (30 (push (list j i) blocks)) ;; 壁
	    (40 (push (list j i) walls)) ;; 壊せない壁
	    (5  (push (list j i) boss)) ;;ボス
	    (3  (push (list j i) items)) ;; 宝箱
	    (2  (push (list j i) kaidan)) ;; 下り階段
	    (6  (push (list j i) events)) ;; イベント
	    (7  (push (list j i) ha2)))))) ;; 中ボス ハツネツエリア
    (list :|blocks| blocks :|walls| walls :|items| items :|boss| boss :|kaidan| kaidan
			   :|events| events :|ha2| ha2)))

;;壁破壊
(defun kabe-break (map p y x)
  (if (>= (random 10) 3)
      (setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 0)
      (setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 3))
  (decf (player-hammer p)))

(defun hummer-get (p)
  (setf (player-msg p) "「ハンマーを見つけた。」")
  (incf (player-hammer p)))

(defun kusuri-get (p)
  (setf (player-msg p) "「回復薬を見つけた。」")
  (incf (player-heal p)))

;;重み付け抽選-----------------------------------------------
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;lst = *copy-buki*
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))
;;------------------------------------------------------------
;; lst = *copy-buki*
;;*copy-buki*の確率の部分をずらす
(defun omomin-zurashi (lst)
  (let ((buki (mapcar #'car lst))
	(omomi (mapcar #'cdr lst)))
    (setf omomi (butlast omomi))
    (push 10 omomi)
    (mapcar #'cons buki omomi)))

;;---------------------------------------------
;;武器ゲット２ 全アイテムからランダム
(defun item-get2 (p)
  (case (random 7)
    ((0 1 2 5) ;;武器ゲット
     (equip? p (weightpick *copy-buki*)))
    ((3 6) (hummer-get p)) ;;ハンマーゲット
    (4 (kusuri-get p)))) ;;回復薬ゲット

;;プレイヤーの場所更新
(defun update-player-pos (p x y map)
  (setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) (+ 10 (player-num p)))
  (setf (aref map (player-posy p) (player-posx p)) 0)
  (setf (player-posy p) (+ (player-posy p) y)
	(player-posx p) (+ (player-posx p) x)))

;;100階イベント
(defun moge-event (p)
  (if (equal (car (player-buki p)) "もげぞーの剣")
      (progn
        (format t "~%「もげぞーの剣が輝き出し、もげぞうの剣に進化した！」~%")
        (equip-buki (assoc "もげぞうの剣" *event-buki* :test #'equal) p))
      (format t "~%「なにも起こらなかった。」~%")))

;;移動後のマップ更新
(defun update-map (map p y x)
  (case (aref (donjon-map map) (+ (player-posy p) y) (+ (player-posx p) x))
    (30 ;;壊せる壁
     (if (and (> (player-hammer p) 0)
	      (> (- (donjon-tate map) 1) (+ (player-posy p) y) 0)
	      (> (- (donjon-yoko map) 1) (+ (player-posx p) x) 0))
	 (kabe-break (donjon-map map) p y x)))
	 ;;(format t "「そっちには移動できません！！」~%")))
    (40 ;;壊せない壁
     nil)
    (2 ;;くだり階段
     (incf (player-map p))
     (maze map p)
     ;;２階降りるごとにハンマーもらえる
     (if (= (mod (player-map p) 2) 0)
	 (incf (player-hammer p)))
     ;;５階降りるごとに宝箱の確率変わる
     (if (= (mod (player-map p) 5) 0)
	 (setf *copy-buki* (omomin-zurashi *copy-buki*)))
     ;;７階降りるごとに敵のレベル上がる
     (if (= (mod (player-map p) 7) 0)
	 (incf *monster-level*)))
    (3 ;;宝箱
     (item-get2 p)
     (update-player-pos p x y (donjon-map map)))
    (5 ;;ボス
     (update-player-pos p x y (donjon-map map))
     (setf *battle?* t
	   *boss?* 1))
    (6 ;;イベント
     (update-player-pos p x y (donjon-map map))
     (moge-event p))
    (7 ;;中ボス
     (update-player-pos p x y (donjon-map map))
     (setf *battle?* t
           *boss?* 2))
    (otherwise
     (update-player-pos p x y (donjon-map map))
     (if nil; (= (randval 13) 1) ;;敵との遭遇確率
	 (setf *battle?* t)))))

(defun map-move2 (map p ai)
  (when (or *battle?* (= *end* 2))
    (return-from map-move2))

  ;;バトル時と差別化するため先頭にmapってのいれとく。1は特に意味なし
  (let ((json (append (list :|map| 1) (player-list p) (map-data-list map))))
    (format (ai-stream ai) "~a~%" (jonathan:to-json json)) ;;データ送る
    (finish-output (ai-stream ai))) ;;なぞ

  (let ((str (read-line (ai-stream ai)))) ;;データもらう
    (cond
     ((equal str "UP")    (update-map map p -1  0))
     ((equal str "DOWN")  (update-map map p  1  0))
     ((equal str "RIGHT") (update-map map p  0  1))
     ((equal str "LEFT")  (update-map map p  0 -1))
     ((equal str "HEAL")  (use-heal p))
     (t
      (error (format nil "マップモードで無効のコマンド: ~s (~a)" str (ai-name ai)))))
    (format t "~a: ~a~%" (ai-name ai) str) ;;アクション表示
    (sleep *map-delay-seconds*)))

;;エンディング
(defun ending (gamen)
  (let* ((ss (floor (- (get-internal-real-time) *start-time*) 1000))
	 (h (floor ss 3600)) (hoge nil)
	 (m (floor (mod ss 3600) 60))
	 (s (mod ss 60)))
    (if *ha2ne2*
	(progn (format t "~%「あなたは見事もげぞうの迷宮を完全攻略した！」~%")
	       (setf hoge "~%「あなたは見事もげぞうの迷宮を完全攻略した！」~%"))
	(progn (format t "~%「もげぞうを倒したが、逃したハツネツエリアが新たな迷宮を作り出した・・・」~%")
	       (format t "「が、それはまた別のお話。」~%")
	       (setf hoge "~%「もげぞうを倒したが、逃したハツネツエリアが新たな迷宮を作り出した・・・」~%が、それはまた別のお話。」~%")))
    (format t "クリアタイムは~2,'0d:~2,'0d:~2,'0d でした！~%" h m s)
    (setf hoge (concatenate 'string hoge (format nil "クリアタイムは~2,'0d:~2,'0d:~2,'0d でした！~%" h m s)))
    (setf hoge (concatenate 'string hoge (ranking-dialog ss)))
    (setf (text gamen) (format nil hoge))))

;; (defun main-gui (map p gamen)
;;   (cond
;;     ((or (= *end* 2) (player-dead p))
;;      nil)
;;     (*battle?*
;;      (orc-battle p gamen)
;;      (if (= *end* 1) ;;ゲームクリア
;; 	 (ending gamen)))
;;     (t
;;      (map-move map p gamen))))

(defun game-rupu (map players gamen)
  (handler-case
   (loop while *rupu*
	 do
	 ;;(main-gui map p gamen)
	 (setf (text gamen) (show-map2 map players))
	 (loop for p in players
	       for ai in *ais*
	       do
	       (map-move2 map p ai))
	 (if (or (= *end* 1) (= *end* 2))
	     (setf *rupu* nil))
	 (ltk:process-events)) ;;なんかループするとき必要ぽい
   (sb-int:simple-stream-error (c);;AIの終了などによってデータの受け渡しができない。
			       (format t "ストリームエラーが発生しました。~%")
			       (format t "~A~%" c)
			       (setf (text gamen) (format nil "ストリームエラーが発生しました。~%~A~%" c)))
   (end-of-file (c);;AIからデータを受け取ることができない。
		(format t "~A~%" c)
		(setf (text gamen) (format nil "~A~%" c)))))

(defun load-ais (command-lines)
  (setf *ais* (mapcar #'load-ai2 command-lines)))

(defun substitute-ambiguous-width-characters (str)
  (map 'string
       (lambda (c)
	 (case c
	   (#\□
	    #\口)
	   (#\■
	    #\口)
	   (otherwise
	    c)))
       str))

;; ドンジョンの行き止まりリストから何も置かれていない点を選んでプレー
;; ヤーを置く。
(defun random-place-player (map p)
  (flet ((vacantp (pt)
		 (destructuring-bind (y x) pt
		   (case (aref (donjon-map map) y x)
		     (0 t)
		     (3 t)
		     (otherwise nil)))))
    (let ((candidates (loop for pt in (donjon-stop-list map)
			    when (vacantp pt)
			    collect pt)))
      (unless candidates
	(error "プレーヤー~aを置ける場所がありません。" (player-num p)))
      (place-player map p (sample candidates)))))

;; 点 (y x) にプレーヤーを置く。マップの該当のセルにプレーヤー番号がセッ
;; トされ、プレーヤー構造体の posy posx が適切に更新される。
(defun place-player (map player point)
  (destructuring-bind (y x) point
    (setf (aref (donjon-map map) y x) (+ 10 (player-num player))
          (player-posy player) y
	  (player-posx player) x)))

;; リストから要素をランダムに選んで返す。
(defun sample (xs)
  (nth (random (length xs)) xs))

(defun gui-start ()
  (setf *random-state* (make-random-state t))
  (with-ltk
   ()
   (wm-title *tk* "もげRPGAIサーバーGUI")
   (bind *tk* "<Alt-q>"
	 (lambda (event)
	   (declare (ignore event))
	   (return-from gui-start)))
   (set-geometry *tk* 700 550 200 200)
   (configure *tk* :padx 16 :pady 16)
   (let* ((f (make-instance 'frame))
	  (f2 (make-instance 'frame :width 500 :height 400))
	  (f0 (make-instance 'frame))
	  (fr0 (make-instance 'labelframe :width 10 :master f0 :text "AI起動コマンド"))
	  (fr1 (make-instance 'labelframe :width 5 :master f :text "バトルディレイ"))
	  (fr2 (make-instance 'labelframe :width 5 :master f :text "マップディレイ"))
	  (fr3 (make-instance 'labelframe :width 5 :master f :text "もげぞうディレイ"))
	  (fr4 (make-instance 'labelframe :width 500 :height 400 :master f2 :text "画面"))
	  (gamen (make-instance 'label :master fr4 :width 500 :text (format nil "AIを読み込んでください。")
				:font "Takaoゴシック 14 normal"))
	  (ai-entry1 (make-instance 'entry :width 75 :master fr0 :text (get-ai-command-line)))
	  (ai-entry2 (make-instance 'entry :width 75 :master fr0 :text ""))
	  (ai-entry3 (make-instance 'entry :width 75 :master fr0 :text ""))
	  (ai-entry4 (make-instance 'entry :width 75 :master fr0 :text ""))
	  (ai-entries (list ai-entry1 ai-entry2 ai-entry3 ai-entry4))
	  (vals (list 0 1 2 3 4 5 6 7 8 9 10)) ;;ディレイ秒数リスト
	  (battle-d (make-instance 'spinbox :width 5 :master fr1 :from 0 :to 10 :text 3 :relief "solid"
				   ;;初期値設定するためには:valuesではなく:from :toを使って:textで初期値を決める
				   :command (lambda (val) ;;文字列になってる
					      (setf *battle-delay-seconds*
						    (/ (parse-integer val) 10.0)
						    *bds* (/ (parse-integer val) 10.0)))))
	  (map-d (make-instance 'spinbox :width 5 :master fr2 :from 0 :to 10 :text 3
				:command (lambda (val)
					   (setf *map-delay-seconds*
						 (/ (parse-integer val) 10.0)))))
	  (moge-d (make-instance 'spinbox :width 5 :master fr3 :from 0 :to 10 :text 3
				 :command (lambda (val)
					    (setf *boss-delay-seconds*
						  (/ (parse-integer val) 10.0)))))
	  (stop-btn (make-instance 'button :master f :text "ストップ！"))
	  (start-btn (make-instance 'button :master f :text "スタート！"))
	  (clear-btn  (make-instance 'button :master f :text "初期化")))
     (pack f0)
     (pack fr0 :fill :both :side :left)
     (pack (list ai-entry1 ai-entry2 ai-entry3 ai-entry4))
     (pack f)
     (pack f2)
     (pack (list fr3 fr1 fr2 start-btn stop-btn clear-btn) :fill :both :side :left :expand t)
     (pack (list moge-d battle-d map-d) :fill :both :side :left :expand t)
     (pack fr4)
     (pack gamen)
     (let ((players nil) (map nil))
       (setf (command start-btn);;スタートボタン
	     (lambda ()
	       (when (null *ais*)
		 (kill-all-ais)
		 ;; 最初に空欄があった位置で打ち切る。
		 (let ((lines (loop
			       for s in (mapcar #'text ai-entries)
			       until (zerop (length s))
			       collect s)))
		   (load-ais lines))
		 (init-data)
		 (setf players (loop for i below (length *ais*) collect (make-player :num i)))
		 (setf map (make-random-donjon))
		 (loop for p in players do (random-place-player map p)))
	       (setf *rupu* t
		     *battle-end* nil)
	       (game-rupu map players gamen))))
     (setf (command stop-btn) ;;ストップボタン
	   (lambda ()
	     (setf *rupu* nil)))
     (setf (command clear-btn);;初期化ボタン
	   (lambda ()
	     (kill-all-ais)
	     (setf *ais* nil)
	     (init-data)
	     (setf *battle-end* t
		   *rupu* nil)
	     (setf (text gamen) (format nil "初期化されました。~%AIを読み込んでください。"))))
     )))
