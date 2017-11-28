(load "item.lisp" :external-format :utf-8)

(defparameter *tate* 11) ;;マップサイズ11
(defparameter *yoko* 11) ;;11
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *attack* '("突く" "ダブルスウィング" "薙ぎ払う" "待機" "回復薬"))

(defparameter *battle?* nil)
(defparameter *battle-state* 0)
(defparameter *monster-num* 6)
(defparameter *monster-level* 1) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *start-time* 0)
(defparameter *ha2ne2* nil)
(defparameter *copy-buki* (copy-tree *buki-d*))
(defparameter *proc* nil)
(defparameter *ai* nil)
(defparameter *ai-name* nil)
(defparameter *ai-atama* nil)
(defparameter *ai-command-line* nil)

(defparameter *battle-delay-seconds* 0.3)
(defparameter *bds* 0.3)
(defparameter *map-delay-seconds* 0.3)
(defparameter *boss-delay-seconds* 0.3)

(defparameter *gamen-clear?* t)
(defparameter *rupu* t)

(defparameter *map100*
  #2A((40 40 40 40 40 40 40 40 40 40 40)
      (40 30 30 30 30 30 30 30 30 30 40)
      (40 30 30 30  0  5  0 30 30 30 40)
      (40 30 30 30  0  0  0 30 30 30 40)
      (40 30 30 30 30  0 30 30 30 30 40)
      (40 30 30 30 30  0 30 30 30 30 40)
      (40 30 30 30 30  0 30 30 30 30 40)
      (40 30 30 30 30  0 30 30 30 30 40)
      (40  3  0  0  0  0  0  0  0  6 40)
      (40 30 30 30 30  1 30 30 30 30 40)
      (40 40 40 40 40 40 40 40 40 40 40)))



(defstruct player
  (hp 30)
  (maxhp 30)
  (agi 30)
  (maxagi 30)
  (str 30)
  (maxstr 30)
  (posy 0)
  (posx 0)
  (map 1) ;;マップ深度
  (heal 2) ;;持ってる薬の数
  (hammer 5) ;;持ってるハンマーの数
  (level 1)
  (exp 0)
  (buki '("なし" 0 0 0))
  (msg nil)
  (item nil) ;;持ち物リスト
  (drop nil) ;;敵からのドロップ品一時保管場所
  (monster-num 0)) ;;戦闘時の敵の総数


(defstruct donjon
  (map nil)  ;;マップ
  (tate 11)  ;;縦幅
  (yoko 11)  ;;横幅
  (stop-list nil)) ;;行き止まりリスト
