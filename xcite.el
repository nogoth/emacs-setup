;;; -*- Emacs-Lisp -*-
;;; exciting cite utility
;;; (c)1996-2009 by HIROSE Yuuji [yuuji@gentei.org]
;;; $Id: xcite.el,v 1.60 2009/10/15 15:46:55 yuuji Exp $
;;; Last modified Fri Oct 16 00:46:12 2009 on firestorm
;;; Update count: 487

;;[Commentary]
;;
;;	This package enables you to register  as many mail/news citation
;;	prefix as you like according to each author, and to select those
;;	headers randomly.
;;
;;	xciteを使うと、メイルやニュースの他者の記事の引用時に、行頭に付
;;	加する発言者を表す 「広瀬>」 のような引用文字列(以後引用タグ)を、
;;	その相手毎に登録できます。引用タグを複数個登録すると引用時にそれ
;;	らをランダムに選ぶことができます。たくさん登録しておくとどれが出
;;	て来るか分からないので、なかなかエキサイティングです(うそ)。C-u
;;	を付けてからxciteを呼ぶと、ミニバッファで複数の引用タグを C-n や
;;	C-p で選択したり、新たなタグで引用することができます。変数の設定
;;	により、「デフォルトで選択、C-uを付けたらランダム」というように
;;	挙動を変更することができます。
;;
;;	xcite は Emacs 上で返事を書くものであれば、mh-e, mew, gnus など、
;;	あらゆるパッケージと組み合わせて使うことが出来ます。また、他のパッ
;;	ケージの引用関数のフックとして働くのではなく、全て自力で引用文を
;;	作成するので、上記のパッケージがなくても動作します。
;;
;;	The latest xcite.el is always available at;
;;	http://www.gentei.org/~yuuji/software/xcite.el
;;
;;[Warning]
;;
;;	You can use this package freely unless you cite your own article
;;	with  a first-person citation  prefix, as  `me' which  will lose
;;	information in the future citation.
;;
;;	xciteはどなたでもご自由にお使い頂けます。ただし、自分自身の記事
;;	を引用するときに、「私>」のような一人称を使わないで下さい。「私>」
;;	などで引用すると、さらに引用されたときに一体誰の記事なのか分から
;;	ず、それならむしろ単純に「>」で引用を繰り返した方がはるかにまし
;;	といえるからです。
;;
;;[Installation]
;;
;;	Put these lines into your ~/.emacs
;;	以下の二つは必ず必要です。
;;
;;		(autoload 'xcite "xcite" "Exciting cite" t)
;;		(autoload 'xcite-yank-cur-msg "xcite" "Exciting cite" t)
;;
;;	and assign your favorite key stroke to these functions.
;;	適当なキーに2関数を割り当てて下さい。以下の例では、C-c C-x に
;;	xciteメニューを、C-c C-y に引用を割り当てます。
;;
;;	(ex.)	(global-set-key "\C-c\C-x" 'xcite)
;;		(global-set-key "\C-c\C-y" 'xcite-yank-cur-msg)
;;
;;	If you are using mh, mew, gnus, etc., set appropriate hook to
;;	bind xcite's functions on their draft buffer.
;;	使っているメイル/ニュースリーダに応じて、以下のようにhookを設定
;;	して下さい。
;;
;;	      (setq mh-letter-mode-hook	; mh-eの場合
;;		    '(lambda ()
;;		       (define-key mh-letter-mode-map "\C-c\C-y"
;;			 'xcite-yank-cur-msg)))
;;	      (setq mew-draft-mode-hook	; mewの場合
;;		    '(lambda ()
;;		       (define-key mew-draft-mode-map "\C-c\C-y"
;;			 'xcite-yank-cur-msg))
;;		    mew-init-hook
;;		    '(lambda ()
;;		       (define-key mew-summary-mode-map "A"
;;			 '(lambda () (interactive)
;;			    (mew-summary-reply)
;;			    (xcite-yank-cur-msg)))))
;;	      (setq news-reply-mode-hook ; GNUS4の場合
;;		    '(lambda ()
;;		       (define-key news-reply-mode-map "\C-c\C-y"
;;			 'xcite-yank-cur-msg)))
;;
;;	If your are using Wanderlust, set like this:
;;	Wanderlustをお使いの場合は以下のようにします。
;;
;;	; Wanderlust 2.6 series
;;	(autoload 'xcite-indent-citation "xcite")
;;	(setq wl-draft-cite-func 'xcite-indent-citation)
;;
;;	; Wanderlust 2.7 or later
;;	(autoload 'xcite-indent-citation "xcite")
;;	(setq wl-draft-cite-function 'xcite-indent-citation)
;;
;;
;;	If you are using GNUS5 or later, such as Semi-gnus, set
;;	appropriately as follows:
;;	Semi-gnusなどのGNUS5以降の版のGnusをお使いの場合は次のように設
;;	定してください。
;;
;;		(autoload 'xcite-indent-citation "xcite")
;;		(setq message-citation-line-function nil
;;		      message-indent-citation-function
;;		      'xcite-indent-citation)
;;
;;	And xcite 1.26 or later recognize "X-cite-me" extensional header
;;	to decide the article author's preferring citation.  If you wish
;;	your correspondent to cite your message with certain citation
;;	prefix, put the "X-cite-me: YourName" header in your mail
;;	draft.  Of course this function will be achieved only when your
;;	correspondent uses new xcite.el.  Here is the sample definition
;;	for mew to put "X-cite-me" header.
;;
;;	xcite 1.26以降では、"X-cite-me:" ヘッダを認識するようにしました。
;;	これは自分がどういう文字列で引用して欲しいかを指定するためのヘッ
;;	ダで、相手がxcite1.26以降を用いているときに自分の文章の引用タグ
;;	を指定できます。以下に、Mew, Wanderlust を用いている場合の
;;	X-cite-me ヘッダの自動挿入設定を示します。
;;
;;		;;for Mew
;;		(setq mew-header-alist '(("X-cite-me:" . "yuuji")))
;;		;;for Wanderlust
;;		(setq wl-draft-config-alist
;;		      '((t
;;			 ("X-cite-me" . "yuuji"))))
;;
;;[How to Use]
;;
;;  * M-x xcite-yank-cur-msg (or C-c C-y)
;;
;;	This  function yanks message   in the next window  with citation
;;	prefix corresponding  to its author.  If you  have not cited the
;;	author's article  ever, Xcite asks  you the citation string with
;;	GCOS name defaulted.    If you give some  string  to the prompt,
;;	that citation string will be used in the next call.
;;
;;	メイルやニュースの原稿を書いているバッファで実行すると、隣のウィ
;;	ンドウに見えている記事を、その発信者に応じた引用タグを付けてカレ
;;	ントバッファにヤンクします。もしその発信者に応じた引用タグを登録
;;	していない場合は引用タグの入力が求められます。ここで引用名を入力
;;	した場合には次回からその引用名を用いた引用タグが挿入されます。何
;;	も入力しなかった場合は、From: 行のGCOS名がデフォルトで用いられま
;;	す。
;;
;;  * C-u M-x xcite-yank-cur-msg (or C-u C-c C-y)
;;
;;	Once you've registered   citation strings to some author,  Xcite
;;	selects one of them at random.  C-u for this function make Xcite
;;	allow you  to  select by C-n  and C-p, or  enter a new  citation
;;	string.  If you enter a new one, it will be added to the list.
;;
;;	特定の発信者に対する引用タグを登録した場合、次回からはそれらの中
;;	からランダムにタグが選ばれて引用されるようになります。C-u を付け
;;	てこの関数を呼ぶと、ミニバッファで複数候補から C-n や C-p で好き
;;	なものを選んだり、新たな引用タグを入力することができます。新しい
;;	ものを入れた場合は、次回からの選択候補に追加されます。
;;
;;  * M-x xcite (or C-c C-x)
;;
;;	This function displays this menu;
;;	以下のメニューが出て来ます。
;;
;;	  Y)ank W)copy A)ppend P)repend I)nsertPrefix R)egionCite Q)fill
;;
;;	`y' is equivalent to M-x xcite-yank-cur-msg.  `C-u' for M-x
;;	xcite will be passed to xcite-yank-cur-msg.
;;	y は M-x xcite-yank-cur-msg (C-c C-y) と同じです。
;;
;;	`w' incorporates marked   region with citation prefix  into yank
;;	buffer.  If you want  to cite more than one  article, you can do
;;	that by visiting   other article to   mark  region you want   to
;;	include and calling  M-x xcite `w'.    The next yank  (C-y) will
;;	paste   that region with  citation  header  and citation prefix.
;;	`C-u' for M-x xcite makes Xcite let you select citation prefix.
;;	w はカレントバッファのマークしたリジョンに引用タグを付けたものを
;;	ヤンクバッファに格納します。もし、一通の記事に複数の記事を引用し
;;	たい場合は、引用したい記事のあるバッファに移動し引用したい範囲を
;;	マークしてこの関数を呼び、書いているバッファに戻ってヤンク(C-y)
;;	すると良いでしょう。
;;
;;	(*1)
;;	`a' and  `p' are  the same with  `w' except  they append/prepend
;;	cited  lines to  kill-ring instead  of replacing  it,  which `w'
;;	command does.   This is handy for citing  messages from multiple
;;	articles.
;;	a と p は、上記 w と同様マークした領域を取り込みますが、ヤンクバッ
;;	ファに追加します(aで後ろに追加、pで先頭に追加)。複数の記事から一
;;	度に引用するときに使うと便利です。
;;
;;	`i' inserts the current citation prefix in the current line.
;;	i は現在の引用タグをカレント行に挿入します。
;;
;;	`r' does the same as `i' on each line in the region.
;;	r はマークしたリジョンの各行に対し i と同様引用タグを挿入します。
;;
;;	`q' fills the current cited paragraph.
;;	q は引用されたパラグラフを引用タグを考慮してfillします。
;;
;;[FAQ]
;;
;;  * Lines which contains `>' or blank lines aren't cited.
;;
;;	Yes, that's the  specification.  That is  the style of named tag
;;	citation.   Lines  which  is already   cited  with author's name
;;	should not be cited twice or more.   But sometimes xcite ignores
;;	a line which simply contains  `>', and which  is not cited.   In
;;	this case, insert citation tag manually by M-x xcite and `i'.
;;
;;  * 空行や「>」を含む行に引用タグが挿入されない
;;
;;	仕様です。名前付きタグで引用することの目的は、発言者を分かりやす
;;	くするためと、引用記号が幾重にもなって見づらくなるのを防ぐことで
;;	すから、既にタグの付いている部分にさらにタグを付けると余計見づら
;;	くなってしまいます。それなら単純に名前を付けず「>」だけで引用し
;;	たほうがましだと言えます。また、空白行に名前付きタグを付けること
;;	もうるさくて見づらいのでxciteでは行いません。場合によっては、単
;;	に「>」文字があるだけで、既に引用されているわけではない行をxcite
;;	が無視してしまう場合がありますが、そのときは M-x xcite i により
;;	手動で引用タグを付けてください。ただし、xciteでもタグの名前を省
;;	略して「>」だけで引用した場合は、全ての行にもれなく「>」を付けて
;;	引用しますので、traditionalな方法がふさわしい場合は名前無しで引
;;	用してください。
;;
;;  * Can I incorporate this program into Debian package?
;;
;;	Yes.
;;	This  "Yes"  is  NOT  a  special answer  only  for  Debian.
;;	My  recognition  on  `free  software'  is  not  the  permanently
;;	constant  notion.  Therefore  I won't  define the  fixed license
;;	sentences at any moment of my life.  All I can say now is I hope
;;	the free  software be; freely  usable, freely (re-)distributable
;;	without  any charge  for  itself, freely  modifiable unless  the
;;	original  author(=me)'s copyrights  are infringed  or neglected,
;;	absolutely not responsible to  any result from itself.  If there
;;	is A  license clauses which  implies these points above  in some
;;	era, this  software can  be classified into  the group  that the
;;	clauses want to assume as `free'.
;;
;;[Customization]
;;
;;  * xcite:insert-header-function
;;
;;	The function    which produces citation    header.  The  default
;;	function is `xcite-default-header'.  Consult this function as an
;;	example.  Define   your favorite function and  set   the name of
;;	function to this variable.  If  you make a function named `foo',
;;	(setq xcite:insert-header-function 'foo).
;;
;;	引用の先頭のヘッダとなる文字列を生成する関数。デフォルトの関数の
;;	xcite-default-header を参考にして下さい。独自の関数を例えば foo
;;	という名前で定義したら、(setq xcite:insert-header-function 'foo)
;;	としてください。
;;
;;  * xcite:cite-hook
;;
;;	The hook function which is called when the article has just been
;;	cited with prefix.  When the hook  function is called, buffer is
;;	narrowed from the beginning of the cited text to the its end.
;;
;;	発言を引用したあとに、引用部分全体に対して働くフック関数。この関
;;	数が呼ばれる時は、そのバッファが引用開始位置から終了位置までのリ
;;	ジョンにnarrowingされています。
;;
;;	ちなみに作者は、自分の記事を「広瀬さん>」と敬称付きで引用される
;;	のが好きでないので、このhookで「さん」を取り除いています。さらに、
;;	漢字の「＞」で引用して来た人の文章の引用タグを取り除いています。
;;	設定は以下のようになります。
;;
;;		;; 「ゆうじさん>」を 「ゆ>」に、
;;		;; 「広瀬さん>」「広瀬氏>」を 「広>」に変える
;;		(setq xcite:cite-hook
;;		      '(lambda ()
;;			 (goto-char (point-min))
;;			 (replace-regexp "^ゆうじさん>" "ゆ>")
;;			 (goto-char (point-min))
;;			 (replace-regexp "^広瀬\\(さん\\|氏\\)>" "広>")
;;			 (goto-char (point-min))
;;			 (replace-regexp
;;			 (concat "^" xcite:current-citation-prefix "＞") ">")))
;;
;;  * xcite:toggle-ask-citation-default
;;
;;	The default action of xcite.el is to select a citation header at
;;	random.  If you want xcite to ask a header, set this variable to
;;	t.  If t,  xcite asks  normally and   select randomly  with  C-u
;;	prefix.
;;
;;	通常は、「デフォルトでランダムで引用タグを決めて、C-uを付けて関
;;	数を起動すると聞いて来る」ですが、これを反転します。
;;
;;  * xcite:mail-buffer-identifier
;;
;;	By   default, xcite detects   the    mail displaying buffer   by
;;	searching  the `Subject:' field.   This variables alters it.  If
;;	you want  to check the mail buffer   by `Date:' field,  set this
;;	variable to "^Date:".
;;
;;	xciteはデフォルトでメイルバッファであることの確認を「Subject:」
;;	フィールドを探して行います。これを別のフィールドに変更します。
;;	「Date:」フィールドで確認させたい時は、この変数を "^Date:" とし
;;	ます。
;;
;;  * xcite:citation-table
;;
;;	The file name in which xcite stores citation name vs. its
;;	author.  Default value is `~/.xciterc'.
;;
;;	著者と引用タグの対応表を保存するファイル名。デフォルト値は
;;	"~/.xciterc"。
;;
;;  * xcite:minibuf-ease-C-k
;;
;;	When  reading citation  prefix  in the  minibuffer, set  initial
;;	point at  the begining of  default string(this is easy  to erase
;;	string  by C-k).   The defualt  value is  yes(non-nil).   If you
;;	prefer the initial position being at the end of string, set this
;;	to nil.
;;
;;	ミニバッファでの引用タグの入力時に、カーソル位置を先頭に置くか
;;	(先頭に置くとC-kで消しやすい)。末尾に置きたいときはこの変数をnil
;;	にする。
;;
;;[Acknowledgements]
;;
;;	まず桂川直己君に感謝します。彼なくしてはxciteは生まれなかったで
;;	しょう。ASCII NETでコロコロとハンドルを変えるので、引用タグをた
;;	くさん切替えたいという動機が生まれました。xciteをfjに公表して感
;;	謝の意を伝えて間もない1997年3月10日、彼は交通事故により26歳でこ
;;	の世を去りました。xciteを使うときに、そのきっかけを作った彼の御
;;	冥福を祈念して頂ければ幸いです。彼が残したハンドルのいくつかを、
;;	xciterc形式で http://www.gentei.org/~yuuji/lune/handles
;;	に置きます。
;;
;;	sc-registerの作者であり、私に Emacs-Lispを教えてくださった廣瀬陽
;;	一さんに感謝します。やはりsc-registerの存在がなかったらxciteも存
;;	在していなかったでしょう。魚がしにて「今はxcite使ってるよ」と聞
;;	いたことは至福の喜びです。
;;
;;	そしてxciteのデバッグやチューンに協力してくださった以下の皆さん
;;	に感謝します。
;;	・油谷竜志郎さん(asciinet)
;;	・和田啓二さん(横浜国大)
;;	・青木昭雄さん(アルファシステムズ)
;;	・宮崎晋さん(九州大)
;;	・亀井達也さん(東工大)
;;	・横田和也さん(マツダ)
;;	・郡山直大さん(日本総合システム)
;;	・森川修さん(NTTコムウェア)
;;	・白井秀行さん(松下電送システム)
;;	・土屋雅稔さん(京都大学)
;;	・新堂安孝さん(大阪市立大学)
;;
;;[Disclaimer]
;;
;;	  This  program is  distributed  as a  free  software.  You  can
;;	use/copy/modify/redistribute  this software  freely but  with NO
;;	warranty  to  anything  as  a  result of  using  this  software.
;;	Especially, I am not responsible for the case when you cite your
;;	friend's  mail  with  a  silly  citation  prefix  in  a  serious
;;	situation :)
;;
;;	The pronunciation of xcite is the same as excite.
;;
;;	このプログラムはフリーソフトウェアです。このプログラムを用いた結
;;	果に対する保証は一切負いませんのでご注意下さい。とくに、友人の発
;;	言をお間抜けなタグで引用したものを、fjに出してしまったりしても私
;;	は関知しません(笑)。また、xciteはいじる気力がすぐになくなる危険
;;	性が高いので、使ってみて要望などがあるときは早めに作者に言って下
;;	さい:-)。既に何人かの人に使ってもらって、各人の好みの折衷になる
;;	ようチューンしてあるので、採用されない要望もあるかと思いますが、
;;	真剣に対応しますので御遠慮無しにどうぞ。
;;
;;	xciteはexciteと同じ発音で読んでください。
;;
;;				Oct. 2009 広瀬雄二 [yuuji@gentei.org]
;;

;;;
;; Exciting cite for Mule
;;;
(defvar xcite:author-type-n "^Author: \\([a-z][a-z][a-z][0-9]+\\) (\\(.*\\))$")
(defvar xcite:author-mail   "^From: \\([a-z][a-z][a-z][-0-9].+\\) (\\(.*\\))$")
(defvar xcite:author-inet   "^From: \\(.*\\S \\)? *<\\(.*\\)>$")
(defvar xcite:author-news   "^From: \\(.*\\S \\) *(\\(.*\\))$")
(defvar xcite:author-vague  "^From: \\(.*\\)$")
(defvar xcite:mail-stamp
  (concat
   "^From \\([a-z][a-z][a-z][0-9]+\\) "		;ID(1)
   "\\([A-Z][a-z][a-z]\\) "			;day of week(2)
   "\\([A-Z][a-z][a-z]\\) *"			;month(3)
   "\\([0-9][0-9]*\\) *"			;day of month(4)
   "\\([0-9][0-9]*\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\) *" ;hh(5):mm(6):ss(7)
   "\\([0-9][0-9][0-9][0-9]\\)"			;year(8)
   ))
(defvar xcite:inet-date "^Date: \\(.*\\)$")
(defvar xcite:inet-msgid	"^Message-Id: \\(<.*>\\)")
(defvar xcite:inet-subject "^Subject: \\(.+\\)$")
(defvar xcite:inet-ng "^Newsgroups: \\(.+\\)$")

(defvar xcite:author-regexp
  (concat
   xcite:author-type-n	"\\|"
   xcite:author-mail	"\\|"
   xcite:author-inet	"\\|"
   xcite:author-news	"\\|"
   xcite:author-vague)
  "Regexp of author header")

(defvar xcite-handle-alphabets "[-a-zA-Z0-9'@_()ー　-龠]"
  "*List of characters that can be used for handle")
(defvar xcite:toggle-ask-citation-default nil
  "*Non-nil means toggle whether xcite asks the citation prefix.
Normally xcite asks the prefix and C-u for xcite determines randomly,
Non-nil for this variable toggles it.")
(defvar xcite:mail-buffer-identifier "^Subject:"
  "*Use this regexp for searching mail buffer which will be cited.")
(defvar xcite:x-cite t)
(defvar xcite:minibuf-ease-C-k t
  "*Non-nil for putting point at the beginning of default citation.")

(defvar xcite-cite-regexp
  (concat
   "^>+[ \t]*\\|"
   "^\\(" xcite-handle-alphabets "\\|[ \t]\\)*" xcite-handle-alphabets ;;"\\)"
   ">+[ \t]*")
  "*Regexp of string which is regarded that it has been already cited.")

(defvar xcite:insert-header-function 'xcite-default-header
  "引用の先頭につける文字列を生成する関数の名前。
その関数中では id, author, date, time, year, month, day, hour, minute, ampm,
tag という変数を利用することができる。ただし、Internet での Mail では
date, id(e-mail アドレス), handle(実名フィールド), msgid(Message-ID),
subject, tag(引用タグ), ng(NetNewsの場合のNewsgroups)が利用可能.")

(defvar xcite:default-user-prefix-alist
  '(("yuuji@ae.keio.ac.jp" "ゆ" "広")
    ("yuuji@gentei.org" "ゆ" "広")
    ("yokota.k@lab.mazda.co.jp" "横田")
    ("yokota-k@venus.dtinet.or.jp" "か")
    ("morikawa.osamu@d70.node.nttcom.co.jp" "森"))
  ".xciterc に無いときに用いられる特別なユーザ引用文字列のリスト")

(defun xcite-default-header ()
  "デフォルトの引用ヘッダ文字列生成関数."
  (if (string-match "jp$" id)
      (format ">>> %s の刻に%s\n>>> %s%s氏曰く\n"
	      date
	      (if (string< "" tag) (concat " 「" tag "」、すなわち") "")
	      id (if handle (concat "(" handle ") ") ""))
    (format ">>>>> On %s\n>>>>> %s%s said:\n"
	    date (or id "") (if handle (concat "(" handle ") ") ""))))



;(defvar xcite:single-article-modes
;  '(mh-show-mode gnus-article-mode mew-message-mode)
;  "*Major modes of which buffers have contain single article;
;If article buffer is in such modes, Xcite searches the author from
;the beginning of the buffer.")
(defvar xcite:multiple-article-modes
  '(anmode)
  "*Major modes of which buffers have contain multiple article;
If article buffer is in such modes, Xcite searches the author from point.")

(defvar xcite:citation-table
  (if (eq system-type 'ms-dos) "~/_xciterc" "~/.xciterc"))
(defvar xcite:citation-suffix "> "
  "*Citation suffix of each tag.  The default value is \"> \".")
(defvar xcite:citation-leader nil
  "*Citation leader of each tag.  The default value is nil.
This string should consist of characters listed in xcite-handle-alphabets.
If you got reset the value of xcite:citation-leader, try to change the
string to more orthodox one.")
(defvar xcite:current-citation-prefix nil
  "Holds the current citation prefix.")

(defvar xcite:get-article-buffer-function nil
  "*User defined function to get mail/news article buffer.
This function should return the buffer object if it found article buffer.
And return nil if not.")

(defvar xcite:paragraph-separate
  (concat paragraph-separate "\\|"
	  xcite:author-regexp))
(defvar xcite:citation-map nil)
(defvar xcite:first-second-person
  "^\\(私\\|[あわ]たく?し\\|僕\\|ぼく\\(ちん\\)?\\|俺\\|おれ\\|me\\|わし\\|あっし\\|おいら\\|拙者?\\|自分\\|己\\|朕\\|余\\|あなた\\|君\\|おまえ\\|[きち]み\\|小生\\|我輩?\\)$"
  "Regexp of the first or second person.")

(if xcite:citation-map nil
  (setq xcite:citation-map (copy-keymap global-map))
  (define-key xcite:citation-map "n"	'xcite-next-line)
  (define-key xcite:citation-map "p"	'xcite-prev-line)
  (define-key xcite:citation-map "v"	'xcite-scroll-up)
  (define-key xcite:citation-map "V"	'xcite-scroll-down)
  (define-key xcite:citation-map " "	'exit-recursive-edit)
  (define-key xcite:citation-map "\^M"	'exit-recursive-edit)
  (define-key xcite:citation-map "q"	'abort-recursive-edit)
  (define-key xcite:citation-map "\^G"	'abort-recursive-edit))
(defvar xcite:cite-message
  "`N' for next-line, `P' for prev-line, RET or SPC for select, Q for quit")

(defun xcite-next-line (arg)
  (interactive "p") (next-line arg) (message xcite:cite-message))
(defun xcite-prev-line (arg)
  (interactive "p") (xcite-next-line (- arg)))
(defun xcite-scroll-up (arg)
  (interactive "p") (scroll-up arg) (message xcite:cite-message))
(defun xcite-scroll-down (arg)
  (interactive "p") (xcite-scroll-up (- arg)))

(defun xcite (arg)
  "sc-regs.def clone for Mule."
  (interactive "P")
  (message "Y)ank W)copy A)ppend P)repend I)nsertPrefix R)egionCite Q)fill")
  (let ((c (downcase (read-char))))
    (cond
     ((= c ?y) (xcite-yank-cur-msg arg))
     ((= c ?w) (let ((xcite:get-article-buffer-function 'current-buffer))
		 (xcite-cite t nil nil arg)))
     ((= c ?a) (let ((xcite:get-article-buffer-function 'current-buffer))
		 (xcite-cite 'append nil nil arg)))
     ((= c ?p) (let ((xcite:get-article-buffer-function 'current-buffer))
		 (xcite-cite 'prepend nil nil arg)))
     ((= c ?i) (xcite/prefix))
     ((= c ?r) (xcite/prefix-region (region-beginning) (region-end)))
     ((= c ?q) (xcite-fill arg)))))

(defun xcite/tr-string (str from to)
  (let ((str (copy-sequence str))(i 0) (len (length str)))
    (while (< i len)
      (if (= (aref str i) from)
	  (aset str i to))
      (setq i (1+ i)))
    str))

(defun xcite/id2prefix (id &optional default ask special)
  "Return the citation prefix for ID with query.
Optional 2nd argument DEFAULT is used as default prefix.
3rd argument RANDOM determines citation prefix at random.
4th argumetn SPECIAL specifies the special default prefix."
  (let (prefix abort (plist (if special (list (list special))))
	default-alist candidates m (random (not ask)))
    (if xcite:toggle-ask-citation-default
	(setq random (not random)))
    (save-excursion
      ;;(if default (xcite/tr-string default ? ?_))
      (set-buffer (find-file-noselect xcite:citation-table))
      (goto-char (point-min))
      (if (and (re-search-forward (concat "^" (regexp-quote id) "\\s ") nil t)
	       (progn
		 (skip-chars-forward " \t")
		 (not (eolp))))
	  ;;return value
	  (save-restriction
	    (save-excursion
	      (narrow-to-region (point) (progn (forward-line 1) (point)))
	      (goto-char (point-min))
	      (while (re-search-forward "\\([!-z　-龠]+\\)\\s *" nil t)
		(setq plist
		      (cons (list (xcite/tr-string (xcite-match-string 1) ?_ ? ))
			    plist))))))
      (while (and default (string< "" default))
	(setq default-alist
	      (cons
	       (list
		(substring
		 default 0
		 (setq m (string-match
			  "\\(\"\\|\\s \\|\\s(\\|\\s)\\)+" default))))
	       default-alist))
	(if m (setq default (substring default (match-end 0)))
	  (setq default nil)))
      (and (null plist)		;Special users' prefix
	   (assoc id xcite:default-user-prefix-alist)
	   (setq plist ;convert to alist
		 (mapcar (function (lambda (e) (list e)))
			 (cdr (assoc id xcite:default-user-prefix-alist)))))
      (or (assoc (substring id 0 (string-match "@" id)) plist)
	  (setq default-alist
		(cons (list (substring id 0 (string-match "@" id)))
		      default-alist)))
      (setq candidates (append (setq plist (reverse plist))
			       default-alist))
      (if (bobp)
	  (progn (goto-char (point-max))
		 (if (not (bolp)) (newline 1))))
      (unwind-protect
	  (if (and random (> (length plist) 0))
	      (setq prefix
		    (car (nth (random (length plist)) plist)))
	    (let*((minibuffer-completion-table candidates)
		  (map minibuffer-selection-complete-map)
		  (language (downcase current-language-environment))
		  (upkey (key-description
			  (car (where-is-internal
				'previous-history-element map))))
		  (dnkey (key-description
			  (car (where-is-internal
				'next-history-element map))))
		  (kill-ring kill-ring));preserve kill-ring
	      (setq random nil)		;turns to flag
	      (while (null prefix)
		(setq prefix
		      (read-from-minibuffer
		       (format "%s(%s/%s): "
			       (cond
				((string-match "japan" language)
				 "引用名")
				((string-match "^chin" language)
				 "Citation Name")
				(t "Citation Name"))
			       (or upkey "C-p") (or dnkey "C-n"))
		       (if (and (string< "19" emacs-version)
				xcite:minibuf-ease-C-k)
			   (cons (car (car candidates))
				 (if (string< "20" emacs-version) 0 1))
			 (car (car candidates)))
		       minibuffer-selection-complete-map))
		(if (string-match xcite:first-second-person prefix)
		    (progn
		      (ding)
		      (message "一・二人称で引用しちゃダメ")
		      (sit-for 2)
		      (setq prefix nil))))))
	(if (or (null prefix)
		(string= prefix "")
		(assoc prefix default-alist)
		(equal prefix (car (car plist)))
		random
		(not (y-or-n-p (concat prefix " を登録しますか?"))))
	    nil
	  (if (assoc prefix plist)
	      (setq plist		;move current choice to the top
		    (cons (list prefix)
			  (delete-member (list prefix) plist)))
	    ;;(setq plist (cons (list prefix) candidates))
	    (setq plist (cons (list prefix) plist)))
	  (delete-region
	   (progn (beginning-of-line) (point))
	   (progn (forward-line 1) (point)))
	  (insert (format "%s\t%s\n" id
			  (mapconcat
			   (function
			    (lambda (s) (xcite/tr-string (car s) ? ?_)))
			   plist " ")))
	  (beginning-of-line)
	  (delete-blank-lines)))
      (basic-save-buffer)
      (kill-buffer nil)
      prefix)))

;;;
;; Minibuffer selection map
;;;
(defvar minibuffer-selection-complete-map nil)
(if minibuffer-selection-complete-map nil
  (setq minibuffer-selection-complete-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key minibuffer-selection-complete-map "\C-p"
    'minibuffer-selection-previous)
  (define-key minibuffer-selection-complete-map "\C-e"
    'minibuffer-selection-previous)
  (define-key minibuffer-selection-complete-map "\C-n"
    'minibuffer-selection-forward)
  (if (eq (key-binding " ") 'mlh-space-bar-backward-henkan)
      (define-key minibuffer-selection-complete-map " " nil))
  (if (string= (getenv "USER") "yuuji")
      (define-key minibuffer-selection-complete-map "\C-x"
	'minibuffer-selection-forward)))

(defun minibuffer-selection-forward (arg)
  "Move to next candidate."
  (interactive "p")
  (let ((list minibuffer-completion-table)
	(current (if (fboundp 'field-string)
		     (field-string (point-max))
		   (buffer-string)))
	(i 0))
    (while list
      (if (string= current (car (car list)))
	  (setq list nil)
	(setq i (1+ i) list (cdr list))))
    (setq i (max (min (+ i arg) (1- (length minibuffer-completion-table))) 0))
    (if (fboundp 'delete-field) (delete-field) (erase-buffer))
    (funcall
     (if xcite:minibuf-ease-C-k
	 (function (lambda (s) (save-excursion (insert s))))
       'insert)
     (car (nth i minibuffer-completion-table)))))

(defun minibuffer-selection-previous (arg)
  "Move to previous candidate."
  (interactive "p")
  (minibuffer-selection-forward (- arg)))

(defvar xcite:strict-citation-check t
  "*Non-nil checks if current line is on citation line strictly.")
(defvar xcite:citation-suffix-regexp "> *")

(defun xcite/get-current-prefix ()
  "Get current line's citation prefix.
Assume citation ends with `>'."
  (save-excursion
    (beginning-of-line)
    (let ((b (point)) e)
      (skip-chars-forward "^>\n")
      (if (eolp)
	  nil
	(setq e (point))
	(if (looking-at ">[> \t]*>")
	    (setq e (+ e (1- (length (xcite-match-string 0))))))
	(xcite-buffer-substring b e)))))

(defun xcite/collect-cite-prefix ()
  "Collect citation prefix on buffer."
  (save-excursion
    (let (px list pxcons)
      (xcite/beginning-of-article)
      (while (re-search-forward xcite-cite-regexp nil t)
	(if (setq px (xcite/get-current-prefix))
	    (if (setq pxcons (assoc px list))
		(if (and (cdr pxcons) (numberp (cdr pxcons)))
		    (setcdr pxcons (1+ (cdr pxcons)))
		  ())
	      (setq list (cons (cons px 1) list))))
	(end-of-line))
      (put 'xcite/collect-cite-prefix 'time (nth 1 (current-time)))
      (put 'xcite/collect-cite-prefix 'cite list))))

(defun xcite/on-citation-p ()
  "Check if current line is on citation or not.
Call (xcite/collect-cite-prefix) before using this function!"
  (if xcite:strict-citation-check
      (let*((db (get 'xcite/collect-cite-prefix 'cite))
	    (px (xcite/get-current-prefix))
	    (a (assoc px db)))
	(and a (numberp (cdr a)) (> (cdr a) 1)
	     (looking-at (concat (car a) xcite:citation-suffix-regexp))))
    (looking-at xcite-cite-regexp)))

(defun xcite/beginning-of-article ()
  "Go to beginning of article according to buffer type.
Return nil if this article has no author information."
  (cond ((memq major-mode xcite:multiple-article-modes)
	 (goto-char (point-min))
	 (re-search-forward "^$" nil 1)
	 (re-search-backward xcite:author-regexp nil t))
	(t (let (hend)
	     (goto-char (point-min))
	     (re-search-forward "^$" nil 1)
	     (setq hend (point))
	     (goto-char (point-min))
	     (if (re-search-forward xcite:author-regexp hend t)
		 (goto-char (match-beginning 0)))))))

(defun xcite-cite (&optional noyank beg end ask buffer)
  "Cite from other buffer.  If NOYANK is non-nil, do not paste the text.
Optional 3rd and 4th argument BEG, END specify the region to cite,
5th argument ASK decides the citation header with query.
BUFFER is the buffer where cited text belongs.
This function should be called from the buffer to which yank-contents
should go."
  (interactive "P")
  (let ((sw (selected-window)) (b (make-marker)) (e (make-marker))
	(cb (current-buffer)) (p (point))
	mesg n id handle date time year month day hour minute ampm msgid tag
	subject ng peoh x-cite-me zmacs-regions
	(tmpbuf " *xcite tmp*"))
    (save-excursion
      (cond
       (noyank nil)
       ((and buffer (get-buffer buffer))
	(set-buffer (get-buffer buffer)))
       ((one-window-p) (switch-to-buffer nil))
       (t (xcite-goto-article-window)))
      (cond
       (beg
	(goto-char (min beg end))
	(setq b (point-marker))
	(goto-char (max beg end))
	(setq e (point-marker)))
       (t
	(set-marker b (region-beginning))
	(set-marker e (region-end))))
      (if (not (eq (marker-buffer b) (marker-buffer e)))
	  (error "そいつぁできない相談だ"))
      (set-buffer (marker-buffer b))
      (setq mesg (xcite-buffer-substring
		  (marker-position b) (marker-position e)))
      (if (null (xcite/beginning-of-article))
	  (error "Can't detect the author of this article."))
      (cond
       ((and (boundp 'an:note-stamp) (looking-at an:note-stamp))
	(setq hour	(xcite-match-string 1)
	      minute	(xcite-match-string 2)
	      ampm	(xcite-match-string 3)
	      month	(xcite-match-string 4)
	      day	(xcite-match-string 5)
	      year	(xcite-match-string 6)
	      date	(xcite-match-string 4 6)
	      time	(xcite-match-string 1 3)
	      id	(xcite-match-string 7)
	      handle	(xcite-match-string 8)))
       ((looking-at xcite:author-type-n)
	(setq id	(xcite-match-string 1)
	      handle	(xcite-match-string 2)))
       ((or (and (looking-at xcite:author-inet)
		 (setq id	(xcite-match-string 2)
		       handle	(or (xcite-match-string 1)
				    (xcite-match-string 2))))
	    (and (looking-at xcite:author-news)
		 (setq id	(xcite-match-string 1)
		       handle	(xcite-match-string 2)))
	    (and (looking-at xcite:author-vague)
		 (setq handle	nil ;"名無しの権兵衛"
		       id	(xcite-match-string 1))))
	(save-excursion
	  (re-search-forward "^$" nil t)
	  (setq peoh (point))		;remark point of end of header
	  (if (re-search-backward xcite:inet-date nil 1)
	      (setq date (xcite-match-string 1)))
	  (goto-char peoh)
	  (if (re-search-backward xcite:inet-msgid nil 1)
	      (setq msgid (xcite-match-string 1)))
	  (goto-char peoh)
	  (if (re-search-backward xcite:inet-ng nil 1)
	      (setq ng (xcite-match-string 1)))
	  (goto-char peoh)
	  (if (re-search-backward xcite:inet-subject nil t)
	      (setq subject (xcite-match-string 1)))))
       ((looking-at xcite:author-mail)
	(setq id	(xcite-match-string 1)
	      handle	(xcite-match-string 2))
	(save-excursion (re-search-backward xcite:mail-stamp))
	(setq month	(xcite-match-string 3)
	      day	(xcite-match-string 4)
	      hour	(xcite-match-string 5)
	      minute	(xcite-match-string 6)
	      year	(xcite-match-string 8)))))
    (select-or-switch-to-buffer cb)	;return to current buffer
    (make-local-variable 'xcite:current-citation-prefix)
    (or (null xcite:citation-leader)
	(string-match
	 xcite-cite-regexp
	 (concat xcite:citation-leader "x" xcite:citation-suffix))
	(setq xcite:citation-leader nil))
    (setq x-cite-me (or (xcite-get-field "X-cite-me:")
			(xcite-get-field "X-attribution:")))
    (setq tag (or (and (not ask) x-cite-me)
		  (xcite/id2prefix id handle ask x-cite-me))
	  xcite:current-citation-prefix
	  (concat xcite:citation-leader tag xcite:citation-suffix))
    (save-excursion			;save-excursion again
      (if noyank
	  (let ((prefix xcite:current-citation-prefix))
	    (set-buffer (get-buffer-create tmpbuf))
	    (setq xcite:current-citation-prefix prefix) ;buffer-local
	    (erase-buffer)
	    (setq p (point))
	    (insert ?\n)))
      (setq b (point-marker))
      (if (fboundp 'insert-and-inherit)
	  (insert-and-inherit mesg)
	(insert-after-markers mesg))
      (setq e (point-marker))
      (goto-char b)
      (if xcite:strict-citation-check
	  (xcite/collect-cite-prefix))
      (while (< (point) (marker-position e))
	(beginning-of-line)
	(if (or (string= xcite:current-citation-prefix xcite:citation-suffix)
		(not (or (looking-at "^\\|$")
			 (looking-at (concat "\\(" tag "\\)>"))
			 ;; (looking-at xcite:paragraph-separate)
			 ;;(looking-at paragraph-separate) ;removed 1.45
			 (xcite/on-citation-p)
			 )))
	    (insert xcite:current-citation-prefix))
	(forward-line 1))
      (save-restriction
	(narrow-to-region b e)
	(run-hooks 'xcite:cite-hook))
      (if (and xcite:insert-header-function
	       (fboundp xcite:insert-header-function))
	  (progn
	    (goto-char p)
	    ;(newline 1)
	    (insert (funcall xcite:insert-header-function))
	    (goto-char p)))
      (if noyank
	  (progn (cond
		  ((eq noyank 'append) (kill-append (buffer-string) nil))
		  ((eq noyank 'prepend) (kill-append (buffer-string) t))
		  (t (copy-region-as-kill (point-min) (point-max))))
		 (kill-buffer tmpbuf))
	;; Hack X-Mailer header (only in yank mode)
	(cond
	 (xcite:x-cite
	  (goto-char (point-min))
	  (cond
	   ((re-search-forward
	    "^\\(X-\\(Mailer\\|Newsreader\\|cite\\)\\|User-agent\\): " b t)
	    (goto-char (match-end 0))
	    (or (looking-at "xcite")
		(insert "xcite" xcite:version "> ")))
	   (t (if (re-search-forward "^Subject: " b t)
		  (re-search-forward "^\\S " b 1))
	      (beginning-of-line)
	      (save-excursion (insert ?\n))
	      (if (pos-visible-in-window-p)
		  (let ((p (point)))
		    (if (fboundp 'original-scroll-up)
			(original-scroll-up 1)
		      (scroll-up 1))
		    (goto-char p)))
	      (insert "X-cite: xcite " xcite:version)))))))))

(defun xcite/prefix ()
  (if (and xcite:current-citation-prefix
	   (stringp xcite:current-citation-prefix))
      (save-excursion
	(beginning-of-line)
	(insert xcite:current-citation-prefix))))

(defun xcite/prefix-region (s e)
  (if (and xcite:current-citation-prefix
	   (stringp xcite:current-citation-prefix))
      (save-excursion
	(save-restriction
	  (narrow-to-region s e)
	  (goto-char s)
	  (while (not (eobp))
	    (xcite/prefix)
	    (forward-line 1))))))

(defun xcite-make-reply ()
  "Make a reply to current buffer article."
  (let (beg)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq beg (point))
      ())))

(defun xcite-yank-cur-msg (&optional arg)
  "Yank current message with citation prefix.
Non-nil for optional argument ARG selects citation prefix randomly."
  (interactive "P")
  (let (beg end b)
    (save-window-excursion
      (run-hooks 'xcite:pre-cite-hook)
      ;;(other-window 1)
      (xcite-goto-article-window)
      (setq b (current-buffer))
      (save-excursion
	(or (and (re-search-forward "^$" nil t)
		 (re-search-backward "^From:" nil t))
	    (progn
	      (goto-char (point-min))
	      (or (re-search-forward "^From:" nil t)))
	    (error "This buffer doesn't seem to be a mail buffer."))
	(re-search-forward "^$")
	(setq beg (point))
	(if (or (re-search-forward "^begin [0-9][0-9][0-9] " nil t)
		;(re-search-forward "^#!/bin/sh" nil t)
		;;/bin/sh excluded 2002/5/30
		)
	    (setq end (progn (forward-line 1) (point)))
	  (setq end (point-max)))))
    (xcite-cite nil beg end arg b)))

(defun xcite-indent-citation ()
  "Alternative function of `message-indent-citation'."
  (let ((cur-buf (current-buffer))
	(tmp-buf (generate-new-buffer " *xcite-indent-citation*")))
    (unwind-protect
	(let ((start (point))
	      (end (mark t)))
	  (set-buffer tmp-buf)
	  (insert-buffer-substring cur-buf start end)
	  (goto-char (point-min))
	  (set-buffer cur-buf)
	  (delete-region start end)
	  (let ((xcite:get-article-buffer-function (lambda () tmp-buf)))
	    (call-interactively 'xcite-yank-cur-msg)))
      (set-buffer cur-buf)
      (kill-buffer tmp-buf))))


(defun xcite-goto-article-window ()
  "Go to mail/news article window"
  (let ((curw (selected-window)) b)
    (or
     (and xcite:get-article-buffer-function
	  (setq b (funcall xcite:get-article-buffer-function))
	  (set-buffer b))
     (and (eq major-mode 'mew-draft-mode) ;Support mew
	  (fboundp 'mew-cache-hit) (fboundp 'mew-buffer-message)
	  (save-excursion
	    (set-buffer (get-buffer-create (mew-buffer-message)))
	    (> (buffer-size) 8))
	  (let ((mew-cite-hook '(lambda ())))
	    (set-buffer (get-buffer-create " *xcite tmp*"))
	    (erase-buffer)
	    (condition-case err
		(mew-draft-cite nil t)
	      (error (mew-draft-cite nil)))
	    (setq major-mode 'mew-message-mode)
	    t))
     (catch 'found
       (while (not (eq (select-window (next-window)) curw))
	 (if (save-excursion
	       (goto-char (point-min))
	       (re-search-forward xcite:mail-buffer-identifier nil t))
	     (throw 'found t))))
     (let ((list (cdr (buffer-list))))
       (catch 'found
	 (while list
	   (set-buffer (car list))
	   (or (string-match "^ " (buffer-name))
	       (if (save-excursion
		     (goto-char (point-min))
		     (re-search-forward xcite:mail-buffer-identifier nil t))
		   (throw 'found (current-buffer))))
	   (setq list (cdr list))))))))

(defun xcite-fill-base (arg)
  (if xcite:strict-citation-check
      (xcite/collect-cite-prefix))    ;update current buffer's prefix db
  (if (xcite/on-citation-p) ;;(looking-at xcite-cite-regexp)
      (let ((fill-prefix (xcite-match-string 0))
	    (paragraph-start xcite:paragraph-separate)
	    (paragraph-separate xcite:paragraph-separate))
	(if (and (string< "21.3.4" emacs-version) (not (featurep 'xemacs)))
	    ;; Emacs-21.3.50's fill-paragraph does not work well
	    (save-excursion
	      (mark-paragraph)
	      (fill-region-as-paragraph (region-beginning) (region-end)))
	  (fill-paragraph arg)))
    (fill-paragraph arg)))

(defun xcite-fill (&optional arg)
  "Alternative function of `fill-paragraph'."
  (interactive "P")
  (save-excursion
    (if (xcite-transient-region-active-p)
	(save-restriction
	  (narrow-to-region (region-beginning) (region-end))
	  (goto-char (point-min))
	  (xcite-fill-base arg))
      (beginning-of-line)
      (xcite-fill-base arg))))

;;;
;; Global subfunctions
;;;
(if (fboundp 'buffer-substring-no-properties)
    (fset 'xcite-buffer-substring
	  (symbol-function 'buffer-substring-no-properties))
  (fset 'xcite-buffer-substring (symbol-function 'buffer-substring)))

(defun xcite-match-string (n &optional m)
  (if (match-beginning n)
      (xcite-buffer-substring
       (match-beginning n) (match-end (or m n)))
    nil))

(defun xcite-match-substring (string n &optional m)
  (if (match-beginning n)
      (substring string (match-beginning n) (match-end (or m n)))
    nil))

(defun select-or-switch-to-buffer (buffer)
  (if (get-buffer-window buffer)
      (select-window (get-buffer-window buffer))
    (switch-to-buffer buffer)))

(if (fboundp 'delete) (fset 'delete-member 'delete)
  (defun delete-member (elt list &optional all)
    "Delete ELT from LIST by side effect.
Non-nil for optional 3rd argument ALL removes all occurences of ELT."
    (let ((ptr (cdr list)) (prevp list) atom)
      (cond
       ((equal elt (car list)) (cdr list))
       (t
	(while ptr
	  (if (equal elt (car ptr))
	      (progn
		(setcdr prevp (cdr ptr))
		(or all (setq ptr nil))))
	  (setq prevp ptr ptr (cdr ptr)))
	list)))))

(defvar xcite:field-case-fold t
  "Non-nil searches header field ignoring the case.")

(defun xcite-get-field (field &optional default-value)
  "Function for user.  Get specified header from article buffer."
  (let (fptn value)
    (or (string-match ":$" field) (setq field (concat field ":")))
    (setq fptn (concat "^" (regexp-quote field) "\\s "))
    (save-window-excursion
      (xcite-goto-article-window)
      (save-excursion
	(goto-char (point-min))
	(let ((case-fold-search xcite:field-case-fold))
	  (re-search-forward (concat "^$\\|" fptn) nil 1))
	(if (looking-at "$")
	    default-value
	  ;; Specified field exists.
	  ;(goto-char (match-end 0))
	  (skip-chars-forward " \t\n")
	  (setq value (xcite-buffer-substring
		       (point)
		       (progn (re-search-forward "^\\S \\|^$" nil 1)
			      (1- (match-beginning 0)))))
	  ;; 改行をスペースに変換すべきかなあ…
	  value)))))

(cond
 ((and (boundp 'transient-mark-mode) (boundp 'mark-active))
  (defun xcite-transient-region-active-p ()
    (and transient-mark-mode mark-active)))
 ((and (boundp 'zmacs-regions) (fboundp 'region-active-p))
  (defun xcite-transient-region-active-p ()
    (and zmacs-regions (region-active-p))))
 (t
  (defun xcite-transient-region-active-p ()
    nil)))

(provide 'xcite)
(defconst xcite:revision "$Revision: 1.60 $"
  "Revision number of xcite.el")
(defconst xcite:version
  (progn (string-match "\\([0-9.]+\\)" xcite:revision)
	 (xcite-match-substring xcite:revision 1)))


; Local variables:
; fill-prefix: ";;	"
; paragraph-start: "^$\\|\\|;;$"
; paragraph-separate: "^$\\|\\|;;$"
; End:
