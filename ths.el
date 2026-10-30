;;; ths.el --- Tonghuashun-style ETAF market dashboard -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (etaf "0.2.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/guidao/emacs-ths

;;; Commentary:
;; M-x ths opens a market UI backed by free public HTTPS data providers.
;; ETAF owns layout, rendering, reactive updates, focus and pointer events.

;;; Code:
(require 'etaf)
(require 'ths-data)
(require 'ths-watchlist)
(require 'ths-search)
(require 'ths-chart)
(require 'ths-theme)

(defcustom ths-buffer-name "*同花顺*"
  "Name of the market dashboard buffer." :type 'string :group 'ths)
(defvar-local ths--state nil "ETAF state ref belonging to this buffer.")
(defvar ths-mode-map)
(declare-function meow-motion-mode "meow-core" (&optional arg))
(defvar-local ths--ranking-pending-focus nil
  "Ranking key and first/last edge awaiting keyboard focus after a page load.")
(defvar-local ths--ranking-toolbar-stock nil
  "Last focused ranking key and stock, retained while using toolbar buttons.")

(defcustom ths-refresh-interval 30
  "Seconds between automatic refreshes; nil disables the timer."
  :type '(choice (const nil) natnum) :group 'ths)
(defcustom ths-auto-fetch t
  "Whether opening THS starts network requests."
  :type 'boolean :group 'ths)
(defcustom ths-default-section 'detail
  "Default quote section in narrow windows.
Used when opening THS or returning to quotes with `ths-show-quotes'.
Opening a security explicitly still shows its details.  Wide windows
keep their multi-panel layout.  After changing this option, press 0
in THS to return to the configured default section."
  :type '(choice (const :tag "行情列表" list) (const :tag "个股详情" detail)
                 (const :tag "五档盘口" book) (const :tag "成交记录" trades))
  :group 'ths)

(defun ths--set-watchlist-columns (symbol columns)
  "Validate and set SYMBOL to watchlist COLUMNS, then repaint open dashboards."
  (unless (and (consp columns) (proper-list-p columns)
               (cl-every (lambda (column)
                           (and (proper-list-p column) (= (length column) 3)
                                (memq (car column) '(:code :name :price :change :volume :float-cap
                                                    :amount :turnover :open :high :low :close :time))
                                (stringp (nth 1 column)) (not (string-empty-p (nth 1 column)))
                                (integerp (nth 2 column)) (>= (nth 2 column) 2))) columns))
    (user-error "表头须为非空的 (字段 标题 宽度) 列表，宽度至少为 2"))
  (set-default symbol columns)
  (when (fboundp 'ths--publish)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and ths--state (etaf-runtime-for-buffer buffer)) (ths--publish))))))

(defcustom ths-watchlist-columns
  '((:code "代码" 8) (:name "名称" 10) (:price "最新" 9) (:change "涨跌幅" 9)
    (:volume "成交量" 12) (:float-cap "流通市值" 13) (:amount "成交额" 12) (:turnover "换手率" 9))
  "Watchlist columns in display order, each (FIELD TITLE WIDTH).
WIDTH is in character cells, including a one-cell trailing gap.
Columns that do not fit are hidden from the right; put important columns first.
Customize this option to apply immediately, or use `ths-refit' after `setq'."
  :type '(repeat (list (choice (const :code) (const :name) (const :price) (const :change)
                               (const :volume) (const :float-cap) (const :amount) (const :turnover)
                               (const :open) (const :high) (const :low) (const :close) (const :time))
                       (string :tag "标题") (integer :tag "宽度")))
  :set #'ths--set-watchlist-columns :group 'ths)

(defun ths-configure-watchlist-columns ()
  "Customize the watchlist's column order, titles and widths."
  (interactive)
  (customize-variable 'ths-watchlist-columns))

(defun ths--watchlist-columns (columns)
  "Return the configured column prefix fitting COLUMNS character cells."
  (let ((remaining (max 2 (- columns 4))) fields)
    (catch 'full
      (dolist (field ths-watchlist-columns)
        (when (> (nth 2 field) remaining)
          (unless fields
            (push (list (car field) (nth 1 field) remaining) fields))
          (throw 'full nil))
        (push field fields)
        (setq remaining (- remaining (nth 2 field)))))
    (nreverse fields)))
(defvar-local ths--timer nil)
(defvar-local ths--requests nil)
(defvar-local ths--session nil)
(defvar ths--panel-rows nil "Dynamically bound panel height budget while rendering.")
(defvar ths--roomy nil "Non-nil when panels have room for vertical spacing.")
(defvar-local ths--stock-page-size 20)

(defun ths--ranking-capacity ()
  "Number of ranking rows that fit above the mode line."
  (max 1 (- (or (cdr (etaf-runtime-viewport)) (window-body-height) 40) 11)))

(defun ths--number (value &optional format-string)
  "Format numeric VALUE using FORMAT-STRING, or show --."
  (if (numberp value) (format (or format-string "%.2f") value) "--"))

(defun ths--amount (amount)
  "Format AMOUNT in yuan."
  (cond ((not (numberp amount)) "--")
        ((>= amount 100000000) (format "%.2f亿" (/ amount 100000000.0)))
        ((>= amount 10000) (format "%.2f万" (/ amount 10000.0)))
        (t (format "%.2f" amount))))

(defun ths--source (source)
  "Return a readable provider name for SOURCE."
  (pcase source ('tencent "腾讯财经") ('eastmoney "东方财富") ('sina "新浪财经") ('fund "天天基金") (_ "等待数据")))

(defun ths--stop ()
  "Cancel this dashboard's timer and pending requests."
  (setq ths--session nil ths--ranking-pending-focus nil ths--ranking-toolbar-stock nil)
  (when (timerp ths--timer) (cancel-timer ths--timer))
  (setq ths--timer nil)
  (when (hash-table-p ths--requests)
    (maphash (lambda (_ cancel) (when (functionp cancel) (funcall cancel))) ths--requests)
    (clrhash ths--requests)))

(defun ths--publish ()
  "Notify the reactive view that its buffer's market snapshots changed."
  (etaf-update-value ths--state
                     (lambda (old) (plist-put (copy-sequence old) :revision
                                              (1+ (or (plist-get old :revision) 0))))))

(defun ths--apply-buffer-theme ()
  "Apply the current theme to the dashboard's unused buffer area."
  (setq-local buffer-face-mode-face
              (list :foreground (ths-theme-color 'foreground)
                    :background (ths-theme-color 'background)))
  (buffer-face-mode 1))

(defun ths--theme-changed ()
  "Repaint mounted dashboards without discarding market data or selection."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and ths--state (etaf-runtime-for-buffer buffer))
        (ths--apply-buffer-theme)
        (ths--publish)))))

(add-hook 'ths-theme-change-hook #'ths--theme-changed)

(defun ths-toggle-theme ()
  "Switch all THS dashboards between dark and light colors."
  (interactive)
  (customize-set-variable 'ths-theme (if (eq ths-theme 'dark) 'light 'dark)))

(defun ths--fetch (symbol kind)
  "Request SYMBOL's KIND once; merge only into the owning live dashboard."
  (let ((key (cons symbol kind)) (buffer (current-buffer)) (session ths--session))
    (unless (gethash key ths--requests)
      (puthash key t ths--requests)
      (let ((cancel
             (ths-data-fetch
              symbol kind
              (lambda (result failure)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (when (and session (eq session ths--session)
                               (etaf-runtime-for-buffer buffer))
                      (remhash key ths--requests)
                      (let* ((index (member symbol ths-data-index-symbols))
                             (items (if index ths-data-indices ths-data-stocks))
                             (old (seq-find (lambda (s) (equal symbol (plist-get s :symbol))) items))
                             (new (copy-sequence old))
                             (error-key (intern (format ":%s-error" kind))))
                        (setq new (plist-put new error-key failure))
                        (when result
                          (when (and (eq kind 'intraday) (not (plist-get result :intraday-close))
                                     (plist-get old :time)
                                     (string-prefix-p (plist-get result :intraday-date) (plist-get old :time)))
                            (setq result (plist-put result :intraday-close (plist-get old :close))))
                          (let ((values result))
                            (while values
                              (let ((field (pop values)) (value (pop values)))
                                (setq new (plist-put new
                                                     (pcase field
                                                       (:source (intern (format ":%s-source" kind)))
                                                       (:fetched-at (intern (format ":%s-fetched-at" kind)))
                                                       (_ field)) value))))))
                        (setq items (mapcar (lambda (s) (if (eq s old) new s)) items))
                        (if index (setq ths-data-indices items) (setq ths-data-stocks items)))
                      (ths--publish))))))))
        ;; A transport can fail synchronously (e.g. curl is missing).
        (when (gethash key ths--requests) (puthash key cancel ths--requests))))))

(defun ths--ensure-chart (&optional force)
  "Fetch the selected chart and trades if expired, or when FORCE is non-nil."
  (when (and ths-auto-fetch ths--session
             (eq (plist-get (etaf-value ths--state) :page) 'quotes))
    (let* ((state (etaf-value ths--state))
           (stock (ths-data-find (plist-get state :selected))))
      (when stock
        (dolist (kind (if (ths-data-otc-p stock) '(quote)
                        (list (plist-get state :chart) 'trades)))
          (let ((stamp (plist-get stock (intern (format ":%s-fetched-at" kind)))))
            (when (or force (not stamp) (> (- (float-time) stamp) (if (ths-data-otc-p stock) 900 (or ths-refresh-interval 30))))
              (ths--fetch (plist-get stock :symbol) kind))))))))

(defun ths-refresh (&optional automatic)
  "Refresh market data asynchronously.
AUTOMATIC limits published fund updates to once every fifteen minutes."
  (interactive)
  (when (and ths--session (etaf-runtime-for-buffer (current-buffer)))
    (if (eq (plist-get (etaf-value ths--state) :page) 'ranking)
        (ths--ranking-fetch t)
      (dolist (stock (append ths-data-stocks ths-data-indices))
        (unless (and automatic (ths-data-otc-p stock)
                     (plist-get stock :quote-fetched-at)
                     (< (- (float-time) (plist-get stock :quote-fetched-at)) 900))
          (ths--fetch (plist-get stock :symbol) 'quote)))
      (unless (ths-data-otc-p (ths-data-find (plist-get (etaf-value ths--state) :selected)))
        (let ((ths-auto-fetch t)) (ths--ensure-chart (not automatic)))))
    (ths--publish)))

(defun ths--feed-status ()
  "Describe outstanding requests and quote failures."
  (let* ((items (append ths-data-stocks ths-data-indices))
         (failed (cl-count-if (lambda (s) (plist-get s :quote-error)) items))
         (pending (if ths--requests (hash-table-count ths--requests) 0)))
    (cond ((> pending 0) (format "行情获取中 · %d 项" pending))
          ((> failed 0) (format "%d 项报价失败 · 保留上次数据 · r 重试" failed))
          ((cl-some (lambda (s) (plist-get s :quote-fetched-at)) items) "真实行情 · 时间见各报价")
          (t "等待行情 · r 刷新"))))

(defvar-local ths--rankings nil "Ranking snapshots keyed by universe, sort and page.")
(defvar-local ths--ranking-positions nil
  "Saved page, offset and selected symbol for each universe and sort.")

(defun ths--ranking-key (state)
  "Return the ranking cache key for STATE."
  (list (ths-data-ranking-universe (plist-get state :ranking-universe))
        (plist-get state :ranking-sort) (plist-get state :ranking-page)))

(defun ths--ranking-data (state)
  "Return the cached ranking for STATE, rejecting mixed-universe snapshots."
  (when ths--rankings
    (let* ((key (ths--ranking-key state)) (data (gethash key ths--rankings)))
      (when (seq-some
             (lambda (row)
               (let* ((symbol (plist-get row :symbol))
                      (market (cond ((string-prefix-p "sh" (or symbol "")) 1)
                                    ((or (string-prefix-p "sz" (or symbol ""))
                                         (string-prefix-p "bj" (or symbol ""))) 0))))
                 (not (and symbol
                           (equal symbol (ths-data--ranking-symbol
                                          (plist-get row :code) market
                                          (plist-get state :ranking-universe)))))))
             (plist-get data :rows))
        (setq data '(:error "旧榜单混入其他类别证券，已清除；按 r 重新获取"))
        (puthash key data ths--rankings))
      data)))

(defun ths--ranking-fetch (&optional force)
  "Request the displayed ranking if expired, or when FORCE is non-nil."
  (when ths--session
    (let* ((state (etaf-value ths--state)) (key (ths--ranking-key state))
           (request-key (cons 'ranking key)) (cached (ths--ranking-data state))
           (stamp (plist-get cached :fetched-at))
           (buffer (current-buffer)) (session ths--session))
      (when (and (eq (plist-get state :page) 'ranking)
                 (not (gethash request-key ths--requests))
                 (or force (not stamp) (> (- (float-time) stamp) (or ths-refresh-interval 30))))
        (puthash request-key t ths--requests)
        (let ((cancel
               (ths-data-fetch-ranking
                (plist-get state :ranking-sort) (plist-get state :ranking-page)
                (lambda (result failure)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (and (eq session ths--session) (etaf-runtime-for-buffer buffer))
                        (remhash request-key ths--requests)
                        (puthash key (if result (plist-put result :error nil)
                                       (plist-put (copy-sequence (gethash key ths--rankings)) :error failure))
                                 ths--rankings)
                        (when (and (or failure (null (plist-get result :rows)))
                                   (equal key (car ths--ranking-pending-focus)))
                          (setq ths--ranking-pending-focus nil))
                        (ths--publish)
                        (ths--ranking-restore-focus)))))
                (plist-get state :ranking-universe))))
          (when (gethash request-key ths--requests) (puthash request-key cancel ths--requests)))
        (ths--publish)))))

(defun ths--ranking-select (ref sort &optional page focus-edge)
  "Display ranking SORT at PAGE using REF, optionally focusing FOCUS-EDGE."
  ;; Migrate an old session when reopening the removed volume ranking.
  (when (eq sort 'volume) (setq sort 'amount page 1))
  (ths--ranking-save-position)
  (setq ths--ranking-pending-focus
        (when focus-edge
          (cons (list (ths-data-ranking-universe (plist-get (etaf-value ref) :ranking-universe))
                      sort (or page 1)) focus-edge)))
  (etaf-update-value
   ref (lambda (old)
         (let ((next (copy-sequence old)))
           (setq next (plist-put next :page 'ranking)
                 next (plist-put next :ranking-sort sort))
           (setq next (plist-put next :ranking-offset 0))
           (plist-put next :ranking-page (or page 1)))))
  (when ths-auto-fetch (ths--ranking-fetch))
  (ths--ranking-restore-focus))

(defun ths--ranking-save-position ()
  "Remember the current ranking location before switching view or sort."
  (when (and ths--state (hash-table-p ths--ranking-positions))
    (let* ((state (etaf-value ths--state)) (key (ths--ranking-key state))
           (rows (plist-get (ths--ranking-data state) :rows))
           (selected (and (equal key (car ths--ranking-toolbar-stock))
                          (plist-get (cdr ths--ranking-toolbar-stock) :symbol)))
           (index (cl-position selected rows :test #'equal
                               :key (lambda (row) (plist-get row :symbol))))
           (offset (or (plist-get state :ranking-offset) 0)))
      (when (eq (plist-get state :page) 'ranking)
        (puthash (seq-take key 2)
                 (list :page (plist-get state :ranking-page) :offset offset
                       :symbol (and index (<= offset index)
                                    (< index (+ offset (ths--ranking-capacity))) selected))
                 ths--ranking-positions)))))

(defun ths-ranking-set-universe (universe)
  "Show ranking UNIVERSE, retaining the sort and its previous location."
  (setq universe (ths-data-ranking-universe universe))
  (ths--ranking-save-position)
  (let* ((state (etaf-value ths--state)) (sort (plist-get state :ranking-sort))
         (saved (and ths--ranking-positions (gethash (list universe sort) ths--ranking-positions)))
         (page (or (plist-get saved :page) 1))
         (offset (or (plist-get saved :offset) 0)))
    (setq ths--ranking-toolbar-stock nil
          ths--ranking-pending-focus
          (when saved
            (cons (list universe sort page) (or (plist-get saved :symbol) 'first))))
    (etaf-update-value
     ths--state
     (lambda (old)
       (let ((next (copy-sequence old)))
         (dolist (pair `((:page . ranking) (:ranking-universe . ,universe)
                        (:ranking-page . ,page) (:ranking-offset . ,offset)))
           (setq next (plist-put next (car pair) (cdr pair))))
         next)))
    (when ths-auto-fetch (ths--ranking-fetch))
    (ths--ranking-restore-focus t)))

(defun ths-ranking-cycle-universe ()
  "Cycle between A shares, ETF and LOF rankings, keeping the current sort."
  (interactive)
  (let ((universe (plist-get (etaf-value ths--state) :ranking-universe)))
    (ths-ranking-set-universe (pcase universe ('etf 'lof) ('lof 'stock) (_ 'etf)))))

(defun ths--ranking-focus-row (rows index)
  "Reveal INDEX in ROWS and give its row native keyboard focus."
  (when-let* ((stock (nth index rows)))
    (let ((offset (* (/ index (ths--ranking-capacity)) (ths--ranking-capacity)))
          (buffer (current-buffer)) (session ths--session)
          (key (ths--ranking-key (etaf-value ths--state)))
          (target (intern (concat "rank-" (plist-get stock :symbol)))))
      (unless (equal offset (plist-get (etaf-value ths--state) :ranking-offset))
        (ths--set ths--state :ranking-offset offset))
      (let ((focus
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((runtime (etaf-runtime-for-buffer buffer)))
                     (when (and runtime (eq session ths--session)
                                (equal key (ths--ranking-key (etaf-value ths--state)))
                                (eq 'ranking (plist-get (etaf-value ths--state) :page))
                                (etaf-host-ref-position runtime target))
                       (etaf-focus runtime target)
                       (ths--pin-window-top buffer))))))))
        ;; Toolbar j/k must restore an existing row before calculating its
        ;; neighbor.  Only newly revealed rows need to wait for publication.
        (if (etaf-host-ref-position (etaf-runtime-for-buffer buffer) target)
            (funcall focus)
          (etaf-next-tick focus))))))

(defun ths--ranking-restore-focus (&optional force)
  "Restore a pending selection in its active ranking.
FORCE allows an explicit category click to move focus from its button."
  (when ths--ranking-pending-focus
    (let ((buffer (current-buffer)) (session ths--session)
          (pending ths--ranking-pending-focus))
      (etaf-next-tick
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (eq session ths--session)
                        (eq pending ths--ranking-pending-focus)
                        (etaf-runtime-for-buffer buffer))
               (ths--ranking-restore-focus-now force)))))))))

(defun ths--ranking-restore-focus-now (force)
  "Apply pending ranking focus after the view is published, honoring FORCE."
  (let ((state (etaf-value ths--state)))
    (when (and ths--ranking-pending-focus (eq (plist-get state :page) 'ranking)
               (equal (car ths--ranking-pending-focus) (ths--ranking-key state)))
      (when-let* ((rows (plist-get (ths--ranking-data state) :rows)))
        (let* ((target (cdr ths--ranking-pending-focus))
               (index (cond ((eq target 'last) (1- (length rows)))
                            ((stringp target)
                             (or (cl-position target rows :test #'equal
                                              :key (lambda (row) (plist-get row :symbol))) 0))
                            (t (min (or (plist-get state :ranking-offset) 0) (1- (length rows))))))
               (focused (etaf-focused-host-ref (etaf-runtime-for-buffer (current-buffer)))))
          (setq ths--ranking-pending-focus nil)
          ;; Do not take focus back if the user tabbed to another control
          ;; while this page was loading.
          (when (or force (null focused) (memq focused '(rank-stock rank-etf rank-lof shortcut-u)))
            (ths--ranking-focus-row rows index)))))))

(defun ths-ranking-next-stock (&optional backwards)
  "Focus the next ranked stock, or the previous one with BACKWARDS."
  (interactive)
  (let* ((state (etaf-value ths--state)) (data (ths--ranking-data state))
         (rows (plist-get data :rows)) (count (length rows))
         (page (plist-get state :ranking-page)) (total (plist-get data :total))
         (capacity (ths--ranking-capacity))
         (offset (min (* (/ (or (plist-get state :ranking-offset) 0) capacity) capacity)
                      (* (/ (max 0 (1- count)) capacity) capacity)))
         (focused (etaf-focused-host-ref (etaf-runtime-for-buffer (current-buffer))))
         (index (cl-position focused rows :test #'eq
                             :key (lambda (row) (intern (concat "rank-" (plist-get row :symbol)))))))
    (when rows
      (setq ths--ranking-pending-focus nil)
      (let ((next (if index (+ index (if backwards -1 1))
                    (if backwards (1- (min count (+ offset capacity))) offset))))
        (cond
         ((<= 0 next (1- count)) (ths--ranking-focus-row rows next))
         ((and backwards (> page 1))
          (ths--ranking-select ths--state (plist-get state :ranking-sort) (1- page) 'last))
         ((and (not backwards) total (< (* page ths-data-ranking-page-size) total))
          (ths--ranking-select ths--state (plist-get state :ranking-sort) (1+ page) 'first)))))))

(defun ths-show-ranking ()
  "Show the whole-market ranking page."
  (interactive)
  (ths-ranking-set-universe (plist-get (etaf-value ths--state) :ranking-universe)))

(defun ths-show-quotes (&optional favorites)
  "Show all quotes, or the watchlist when FAVORITES is non-nil."
  (interactive)
  (ths--ranking-save-position)
  (setq ths--ranking-pending-focus nil)
  (etaf-update-value
   ths--state
   (lambda (old)
     (let ((next (copy-sequence old)))
       (setq next (plist-put next :page (if favorites 'watchlist 'quotes))
             next (plist-put next :tab (if favorites 'favorites 'all))
             next (plist-put next :section (when favorites 'list)))
       (when favorites
         (setq next (plist-put next :query "")))
       (let ((stocks (ths--stocks next)))
         (unless (seq-find (lambda (stock)
                            (equal (ths-data-id stock) (plist-get next :selected))) stocks)
           (setq next (plist-put next :selected (ths-data-id (car stocks))))))
       next)))
  (ths--ensure-chart)
  (when favorites (ths--watchlist-focus))
  (when ths-auto-fetch (ths-refresh)))

(defun ths-show-watchlist ()
  "Show the plugin's saved watchlist."
  (interactive)
  (ths-show-quotes t))

(defun ths--watchlist-focus ()
  "Focus the selected watchlist row after the current render."
  (let ((buffer (current-buffer)) (session ths--session))
    (etaf-next-tick
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let* ((state (etaf-value ths--state))
                  (runtime (etaf-runtime-for-buffer buffer))
                  (code (plist-get state :selected))
                  (target (and code (intern (concat "watch-" code)))))
             (when (and runtime (eq session ths--session)
                        (eq (plist-get state :page) 'watchlist)
                        target (etaf-host-ref-position runtime target))
               (etaf-focus runtime target)
               (ths--pin-window-top buffer)))))))))

(defun ths--watchlist-open (ref stock)
  "Open watchlist STOCK's intraday details using REF."
  (etaf-update-value
   ref (lambda (old)
         (let ((next (copy-sequence old)))
           (dolist (pair `((:page . quotes) (:tab . all) (:query . "")
                          (:selected . ,(ths-data-id stock)) (:section . detail)
                          (:chart . intraday)))
             (setq next (plist-put next (car pair) (cdr pair))))
           next)))
  (when ths-auto-fetch
    (ths--fetch (plist-get stock :symbol) 'quote)
    (ths--ensure-chart)))

(defun ths--watchlist-select-focused (ref code)
  "Select focused CODE in REF after row reconciliation finishes."
  (let ((buffer (current-buffer)) (session ths--session))
    (etaf-next-tick
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((runtime (etaf-runtime-for-buffer buffer)) (state (etaf-value ref)))
             (when (and runtime (eq session ths--session)
                        (eq (plist-get state :page) 'watchlist)
                        (eq (etaf-focused-host-ref runtime) (intern (concat "watch-" code)))
                        (not (equal code (plist-get state :selected))))
               (ths--set ref :selected code)))))))))

(defun ths-ranking-next-page (&optional backwards)
  "Go to the next ranking page, or previous when BACKWARDS is non-nil."
  (interactive)
  (setq ths--ranking-pending-focus nil)
  (let* ((state (etaf-value ths--state)) (page (plist-get state :ranking-page))
         (data (ths--ranking-data state)) (total (plist-get data :total))
         (capacity (ths--ranking-capacity))
         (count (length (plist-get data :rows)))
         (offset (min (* (/ (or (plist-get state :ranking-offset) 0) capacity) capacity)
                      (* (/ (max 0 (1- count)) capacity) capacity))))
    (when (eq (plist-get state :page) 'ranking)
      (cond
       ((and backwards (> offset 0)) (ths--set ths--state :ranking-offset (max 0 (- offset capacity))))
       ((and (not backwards) (< (+ offset capacity) count))
        (ths--set ths--state :ranking-offset (+ offset capacity)))
       ((if backwards (> page 1) (and total (< (* page ths-data-ranking-page-size) total)))
        (ths--ranking-select ths--state (plist-get state :ranking-sort) (+ page (if backwards -1 1)))
        (when backwards
          (ths--set ths--state :ranking-offset (* (/ (1- ths-data-ranking-page-size) capacity) capacity))))))))

(defun ths-ranking-previous-page ()
  "Go to the previous ranking page."
  (interactive)
  (ths-ranking-next-page t))

(defun ths-stock-page (&optional backwards)
  "Select the first stock on the adjacent page, or previous with BACKWARDS."
  (interactive)
  (let* ((state (etaf-value ths--state)) (stocks (ths--stocks state))
         (index (or (cl-position (plist-get state :selected) stocks
                                 :key (lambda (stock) (ths-data-id stock)) :test #'equal) 0))
         (page (+ (/ index ths--stock-page-size) (if backwards -1 1)))
         (stock (nth (* (max 0 page) ths--stock-page-size) stocks)))
    (when stock (ths--set ths--state :selected (ths-data-id stock)))))

(defun ths-next-page (&optional backwards)
  "Page through the current stock list or ranking, BACKWARDS when non-nil."
  (interactive)
  (if (eq (plist-get (etaf-value ths--state) :page) 'ranking)
      (ths-ranking-next-page backwards)
    (ths-stock-page backwards)))

(defun ths-previous-page ()
  "Page backwards in the current list."
  (interactive)
  (ths-next-page t))

(defun ths--ranking-open (ref stock)
  "Open ranked STOCK in the quote dashboard belonging to REF."
  (setq ths--ranking-toolbar-stock (cons (ths--ranking-key (etaf-value ref)) stock))
  (ths--ranking-save-position)
  (let ((code (ths-data-id stock)) (symbol (plist-get stock :symbol)))
    (unless (ths-data-find code)
      (setq ths-data-stocks
            (append ths-data-stocks
                    (list (append (copy-sequence stock)
                                  (list :sector (plist-get (ths-data-placeholder symbol) :sector)
                                        :quote-source (plist-get (ths--ranking-data (etaf-value ref)) :source)
                                        :quote-fetched-at (plist-get (ths--ranking-data (etaf-value ref)) :fetched-at)))))))
    (etaf-update-value
     ref (lambda (old)
           (let ((next (copy-sequence old)))
             (dolist (pair `((:page . quotes) (:tab . all) (:query . "") (:selected . ,code) (:section . detail)))
               (setq next (plist-put next (car pair) (cdr pair))))
             next)))
    (when ths-auto-fetch
      (ths--fetch symbol 'quote)
      (ths--ensure-chart))))

(defun ths--volume (volume)
  "Format VOLUME in lots, never as monetary amount."
  (cond ((not (numberp volume)) "--")
        ((>= volume 100000000) (format "%.2f亿手" (/ volume 100000000.0)))
        ((>= volume 10000) (format "%.2f万手" (/ volume 10000.0)))
        (t (format "%.0f手" volume))))

(defun ths--ranking-columns (columns &optional sort universe)
  "Return table fields for COLUMNS cells, keeping SORT's amount visible."
  (cond
   ((and (< columns 80) (memq universe '(etf lof)))
    (append '((:code "代码" 8) (:name "名称" 10) (:price "最新" 8))
            (list (if (eq sort 'amount) '(:amount "成交额" 13) '(:change "涨跌幅" 10)))))
   ((< columns 80)
      (if (eq sort 'amount)
          (append '((:code "代码" 8) (:name "名称" 9) (:amount "成交额" 11) (:float-cap "流通市值" 11))
                  (when (>= columns 55) '((:change-60d "60日涨跌幅" 12))))
        '((:code "代码" 8) (:name "名称" 9) (:float-cap "流通市值" 11) (:change-60d "60日涨跌幅" 12))))
   (t (append
     (if (< columns 90)
         '((:rank "名次" 5) (:code "代码" 8) (:name "名称" 10)
           (:price "最新" 8) (:change "涨跌幅" 9))
       '((:rank "名次" 6) (:code "代码" 9) (:name "名称" 12)
         (:price "最新" 10) (:change "涨跌幅" 11)))
     (list (if (eq sort 'amount)
               (list :amount "成交额" (if (< columns 90) 12 14))
             (list :volume "成交量" (if (< columns 90) 12 14))))
     '((:float-cap "流通市值" 11) (:change-60d "60日涨跌幅" 13))
     (when (>= columns 112)
       (list (if (eq sort 'amount) '(:volume "成交量" 12) '(:amount "成交额" 12))
             '(:turnover "换手率" 10)))))))

(defun ths--ranking-cells (fields &optional stock)
  "Build fixed-width table cells for FIELDS and optional STOCK, or headers."
  (mapcar
   (lambda (field)
     (let* ((key (car field)) (width (nth 2 field))
            (value (plist-get stock key))
            (text (if (not stock) (nth 1 field)
                    (pcase key
                      (:rank (number-to-string value))
                      ((or :price :open :high :low :close) (ths-data-format-price stock value))
                      ((or :change :change-60d) (ths--number value "%+.2f%%"))
                      (:volume (ths--volume value))
                      ((or :amount :float-cap) (ths--amount value))
                      (:turnover (ths--number value "%.2f%%"))
                      (_ (or value "--")))))
            (color (cond ((and stock (eq key :change-60d)) (ths--color value))
                         ((and stock (memq key '(:price :change))) (ths--color (plist-get stock :change)))
                         (t (ths-theme-color 'secondary)))))
       (etaf-node 'box (list :width (list 'ch width) :height '(lh 1) :overflow 'hidden :help-echo text)
                  (list (ths--text (truncate-string-to-width text (1- width)) color))))) fields))

(defun ths--ranking-view (state ref columns)
  "Build the whole-market ranking page for STATE, REF and COLUMNS."
  (let* ((data (ths--ranking-data state)) (rows (plist-get data :rows))
         (universe (ths-data-ranking-universe (plist-get state :ranking-universe)))
         (sort (plist-get state :ranking-sort)) (page (plist-get state :ranking-page))
         (total (plist-get data :total)) (stamp (plist-get data :fetched-at))
         (failure (plist-get data :error))
         (loading (and ths--requests (gethash (cons 'ranking (ths--ranking-key state)) ths--requests)))
         (fields (ths--ranking-columns columns sort universe))
         (capacity (ths--ranking-capacity))
         (offset (min (* (/ (or (plist-get state :ranking-offset) 0) capacity) capacity)
                      (* (/ (max 0 (1- (length rows))) capacity) capacity))))
    (etaf-node
     'column (list :ref 'ranking-frame :height (list 'lh (1- (or (cdr (etaf-runtime-viewport)) (window-body-height) 40)))
                   :overflow 'hidden :bgcolor (ths-theme-color 'background) :color (ths-theme-color 'foreground) :padding '((lh 0) (ch 1)))
     (list
      (etaf-node 'row '(:item-gap (ch 1))
                 (list (ths--button 'page-quotes "行情 0" #'ths-show-quotes)
                       (ths--button 'page-watchlist "自选股 4" #'ths-show-watchlist)
                       (ths--button 'page-ranking "排行榜 3" #'ths-show-ranking t)))
      (ths--shortcut-hint columns t)
      (etaf-node 'row '(:item-gap (ch 1) :height (lh 1) :overflow hidden)
                 (list (ths--button 'rank-stock "A股" (lambda () (ths-ranking-set-universe 'stock)) (eq universe 'stock))
                       (ths--button 'rank-etf "ETF" (lambda () (ths-ranking-set-universe 'etf)) (eq universe 'etf))
                       (ths--button 'rank-lof "LOF" (lambda () (ths-ranking-set-universe 'lof)) (eq universe 'lof))
                       (ths--button 'rank-change "涨幅榜 ↓" (lambda () (ths--ranking-select ref 'change)) (eq sort 'change))
                       (ths--button 'rank-amount "成交额榜 ↓" (lambda () (ths--ranking-select ref 'amount)) (eq sort 'amount))))
      (ths--text (concat (if (eq universe 'stock) "沪深京 A 股" (ths-data-ranking-label universe))
                         (if (eq sort 'amount) " · 按成交额降序 · 单位：元"
                           (if (eq universe 'stock) " · 从高到低 · 成交量单位：手（100 股）"
                             " · 从高到低 · 成交量单位：手（100 份）")))
                 (ths-theme-color 'secondary))
      (ths--text (concat (ths--source (plist-get data :source)) " · "
                         (if stamp (concat "采集时间 " (format-time-string "%F %T" (seconds-to-time stamp) 28800))
                           "尚未获取数据")
                         (cond (loading " · 正在刷新…")
                               (failure (if rows " · 更新失败，保留上次榜单" " · 获取失败，按 r 重试"))
                               (t ""))
                         (when (eq (plist-get data :source) 'sina) " · 60日涨跌幅暂缺"))
                 (if failure (ths-theme-color 'warning) (ths-theme-color 'secondary)))
      (when failure
        (etaf-node 'box (list :help-echo failure)
                   (list (ths--text (truncate-string-to-width failure (max 20 (- columns 6))) (ths-theme-color 'warning)))))
      (ths--panel
       (format "%s / 第 %d 页 / 共 %s 支" (if (eq sort 'amount) "成交额排行" "涨幅排行") page
               (if total (number-to-string total) "--"))
       (append
        (list (etaf-node 'row nil (ths--ranking-cells fields)))
        (cl-loop for stock in (seq-subseq rows offset (min (length rows) (+ offset capacity))) for i from 0
                 collect
                 (etaf-node 'row
                            (list :ref (intern (concat "rank-" (plist-get stock :symbol))) :key (plist-get stock :symbol)
                                  :class (if (cl-evenp i) "ranking-row" "ranking-row ranking-row-alt")
                                  :role 'button :tab-index 0 :height '(lh 1)
                                  :on-focus-enter (let ((selected stock) (key (ths--ranking-key state)))
                                                    (lambda () (setq ths--ranking-toolbar-stock (cons key selected))))
                                  :on-press (let ((selected stock))
                                              (lambda () (ths--ranking-open ref selected)))
                                  :hover-style (list :background-color (ths-theme-color 'selected))
                                  :help-echo (format "查看 %s" (plist-get stock :name)))
                            (ths--ranking-cells fields stock)))
        (unless rows (list (ths--text (if loading "正在获取排行榜…" "暂无排行数据") (ths-theme-color 'warning))))))
      (etaf-node 'row '(:item-gap (ch 2))
                 (list (ths--button 'rank-prev "上一页 p" #'ths-ranking-previous-page)
                       (ths--text (format "%d / %s · 段 %d/%d" page (if total (number-to-string (max 1 (ceiling total ths-data-ranking-page-size))) "--")
                                          (1+ (/ offset capacity)) (max 1 (ceiling (length rows) capacity))))
                       (ths--button 'rank-next "下一页 n" #'ths-ranking-next-page)))))))

(defun ths--initial-state (favorites)
  "Return fresh application state with FAVORITES stock symbols."
  (list :selected (ths-data-id (car ths-data-stocks)) :query "" :tab 'all :chart 'intraday
        :page 'quotes :ranking-universe 'stock :ranking-sort 'change :ranking-page 1
        :sorted nil :favorites (mapcar #'ths-data-symbol-id favorites)))

(defun ths--stocks (state)
  "Return stocks visible under STATE."
  (ths-data-filter (plist-get state :query) (plist-get state :favorites)
                   (eq (plist-get state :tab) 'favorites) (plist-get state :sorted)))

(defun ths--set (ref key value)
  "Set KEY to VALUE in REF, keeping selection consistent with the list."
  (etaf-update-value
   ref (lambda (old)
         (let* ((next (plist-put (copy-sequence old) key value))
                (stocks (ths--stocks next)))
           (unless (seq-find (lambda (stock)
                               (equal (ths-data-id stock) (plist-get next :selected))) stocks)
             (setq next (plist-put next :selected (ths-data-id (car stocks)))))
           next)))
  (when (eq ref ths--state)
    (ths--ensure-chart)
    (when (and (eq (plist-get (etaf-value ref) :page) 'watchlist)
               (memq key '(:selected :query :favorites :sorted)))
      (ths--watchlist-focus))))

(defun ths--color (change)
  "Return the A-share quote color for CHANGE."
  (cond ((null change) (ths-theme-color 'secondary)) ((> change 0) (ths-theme-color 'up)) ((< change 0) (ths-theme-color 'down)) (t (ths-theme-color 'neutral))))

(defun ths--text (text &optional color)
  "Return a text View containing TEXT and optional COLOR."
  (let ((node (etaf-node 'text (when color (list :color color)) (list text))))
    (if ths--panel-rows
        (etaf-node 'box (list :height (list 'lh (1+ (cl-count ?\n text))) :overflow 'hidden :help-echo text)
                   (list node))
      node)))

(defun ths--button (ref label callback &optional selected help compact)
  "Return a button with REF, LABEL, CALLBACK and SELECTED state.
HELP overrides its tooltip; COMPACT removes horizontal padding."
  (etaf-node 'box
             (list :ref ref :role 'button :tab-index 0
                   :on-press callback :padding (if compact '((lh 0) (ch 0)) '((lh 0) (ch 1))) :height '(lh 1) :overflow 'hidden
                   :bgcolor (if selected (ths-theme-color 'selected) (ths-theme-color 'button))
                   :color (if selected (ths-theme-color 'selected-text) (ths-theme-color 'button-text))
                   :hover-style (list :background-color (ths-theme-color 'hover))
                   :help-echo (or help label))
             (list label)))

(defun ths--space (rows)
  "Return a vertical spacer of ROWS text lines."
  (etaf-node 'box (list :height (list 'lh (max 0 rows))) nil))

(defun ths--panel-overhead ()
  "Return rows occupied by panel title, padding and title gap."
  (if ths--roomy 4 1))

(defun ths--book-height ()
  "Return the space needed for all five bid and ask levels."
  (+ (ths--panel-overhead) 12 (if ths--roomy 2 0)))

(defun ths--panel (title children &optional width ref)
  "Return a panel with TITLE, CHILDREN, character WIDTH and optional REF.
Named panels occupy the current height budget, including padding."
  (etaf-node 'column
             (append (list :padding (if ths--roomy '((lh 1) (ch 1)) '((lh 0) (ch 1))) :bgcolor (ths-theme-color 'panel))
                     (when ref (list :ref ref :height (list 'lh ths--panel-rows)))
                     (when width (list :width (list 'ch width))))
             (append (list (ths--text title (ths-theme-color 'secondary))
                           (ths--space (if ths--roomy 1 0))) children)))

(defun ths--favorite (ref)
  "Toggle current stock in REF's watchlist."
  (let* ((state (etaf-value ref)) (code (plist-get state :selected))
         (favorites (plist-get state :favorites)))
    (when code
      (let ((next (if (member code favorites) (remove code favorites)
                    (append favorites (list code)))))
        (ths--save-favorites next)
        (ths--set ref :favorites next)))))

(defun ths--save-favorites (codes &optional new-stock)
  "Persist favorite CODES, allowing a not-yet-inserted NEW-STOCK."
  (ths-watchlist-save
   (mapcar (lambda (code)
             (plist-get (or (ths-data-find code)
                            (and (equal code (ths-data-id new-stock)) new-stock)) :symbol)) codes)))

(defun ths-add-favorite (input &optional name)
  "Add stock INPUT, retaining the watchlist page or opening quote details.
NAME is optional display metadata selected from stock search suggestions."
  (interactive (let ((stock (ths-search-read ths-data-stocks)))
                 (list (plist-get stock :symbol) (plist-get stock :name))))
  (let* ((symbol (ths-watchlist-symbol input)) (code (ths-data-symbol-id symbol))
         (stock (ths-data-find code)) (state (etaf-value ths--state))
         (watchlist (eq (plist-get state :page) 'watchlist))
         (favorites (plist-get state :favorites))
         (next (if (member code favorites) favorites (append favorites (list code)))))
    (when (and stock (not (equal symbol (plist-get stock :symbol))))
      (user-error "列表中已有其他市场的同代码证券：%s" code))
    (unless stock
      (setq stock (ths-data-placeholder symbol))
      (when name (setq stock (plist-put stock :name name))))
    (ths--save-favorites next stock)
    (unless (ths-data-find code) (setq ths-data-stocks (append ths-data-stocks (list stock))))
    (etaf-update-value
     ths--state (lambda (old)
                  (let ((value (copy-sequence old)))
                    (dolist (pair `((:favorites . ,next) (:page . ,(if watchlist 'watchlist 'quotes)) (:tab . favorites)
                                    (:query . "") (:selected . ,code) (:section . ,(if watchlist 'list 'detail))))
                      (setq value (plist-put value (car pair) (cdr pair))))
                    value)))
    (when watchlist (ths--watchlist-focus))
    (when ths-auto-fetch
      (ths--fetch symbol 'quote)
      (ths--ensure-chart))))

(defun ths-remove-favorite ()
  "Remove the selected stock from the watchlist."
  (interactive)
  (let* ((state (etaf-value ths--state)) (code (plist-get state :selected))
         (favorites (plist-get state :favorites)))
    (if (member code favorites)
        (let ((next (remove code favorites)))
          (ths--save-favorites next)
          (ths--set ths--state :favorites next))
      (message "当前股票不在自选中"))))

(defun ths--search (ref)
  "Prompt for a filter and update REF."
  (ths--set ref :query
            (string-trim (read-string "代码 / 名称 / 行业（留空显示全部）: "
                                      (plist-get (etaf-value ref) :query)))))

(defun ths--watchlist-view (state ref columns rows)
  "Build a full-width watchlist table for STATE and REF in COLUMNS by ROWS."
  (let* ((ths--roomy nil) (ths--panel-rows (- rows 5))
         (stocks (ths--stocks state))
         (capacity (max 1 (- rows 7)))
         (index (or (cl-position (plist-get state :selected) stocks
                                 :key #'ths-data-id :test #'equal) 0))
         (page (/ index capacity))
         (visible (seq-subseq stocks (* page capacity)
                             (min (length stocks) (* (1+ page) capacity))))
         (fields (ths--watchlist-columns columns)))
    (setq ths--stock-page-size capacity)
    (etaf-node
     'column (list :ref 'watchlist-frame :height (list 'lh (1- rows)) :overflow 'hidden
                   :bgcolor (ths-theme-color 'background) :color (ths-theme-color 'foreground)
                   :padding '((lh 0) (ch 1)))
     (list
      (etaf-node 'row '(:height (lh 1) :item-gap (ch 1))
                 (list (ths--button 'page-quotes "行情 0" #'ths-show-quotes)
                       (ths--button 'page-watchlist "自选股 4" #'ths-show-watchlist t)
                       (ths--button 'page-ranking "排行榜 3" #'ths-show-ranking)))
      (ths--shortcut-hint columns nil state)
      (ths--text (concat (ths--feed-status)
                        (when (< (length fields) (length ths-watchlist-columns))
                          (format " · %d 列未显示，按 C 配置或加宽窗口"
                                  (- (length ths-watchlist-columns) (length fields)))))
                 (ths-theme-color 'secondary))
      (ths--panel
       (format "自选股 / %d 支%s" (length stocks) (if (plist-get state :sorted) " · 按涨幅排序" ""))
       (append
        (list (etaf-node 'row '(:height (lh 1)) (ths--ranking-cells fields)))
        (cl-loop for stock in visible for i from 0
                 for code = (ths-data-id stock)
                 collect
                 (let ((selected-stock stock) (selected-code code))
                   (etaf-node
                    'row
                    ;; Reuse visible row slots when paging; refs still identify
                    ;; securities for focus and activation.
                    (list :ref (intern (concat "watch-" code))
                          :class (if (cl-evenp i) "ranking-row" "ranking-row ranking-row-alt")
                          :role 'button :tab-index 0 :height '(lh 1)
                          :bgcolor (when (equal code (plist-get state :selected))
                                     (ths-theme-color 'selected))
                          :on-focus-enter
                          (lambda () (ths--watchlist-select-focused ref selected-code))
                          :on-press (lambda () (ths--watchlist-open ref selected-stock))
                          :help-echo (format "回车查看 %s 的分时图" (plist-get stock :name)))
                    (ths--ranking-cells fields
                                        (plist-put (copy-sequence stock) :change (ths-data-change stock))))))
        (unless stocks
          (list (ths--text (if (plist-get state :favorites)
                              "没有匹配股票；按 / 清空搜索。"
                            "暂无自选；按 a 添加股票或基金。") (ths-theme-color 'warning)))))
       nil 'watchlist-table)
      (etaf-node 'row '(:height (lh 1) :item-gap (ch 2))
                 (list (ths--button 'stock-prev "上一页 p" (lambda () (ths-stock-page t)))
                       (ths--text (format "%d / %d" (1+ page) (max 1 (ceiling (length stocks) capacity))))
                       (ths--button 'stock-next "下一页 n" #'ths-stock-page)))))))

(defun ths--watchlist (state ref)
  "Build stock list from STATE, with callbacks updating REF."
  (let* ((stocks (ths--stocks state))
         (capacity (if ths--panel-rows ths--stock-page-size (max 1 (length stocks))))
         (index (or (cl-position (plist-get state :selected) stocks
                                 :key (lambda (stock) (ths-data-id stock)) :test #'equal) 0))
         (page (/ index capacity))
         (row-height (if ths--roomy 3 1))
         (visible (seq-subseq stocks (* page capacity) (min (length stocks) (* (1+ page) capacity))))
         (free (- ths--panel-rows (ths--panel-overhead) 3 (* row-height (length visible))
                  (if stocks 0 1))))
    (ths--panel
     (format "行情列表  /  %d 支" (length (ths--stocks state)))
     (append
      (list
       (etaf-node 'row '(:item-gap (ch 1))
                  (list (ths--button 'all "全部" (lambda () (ths--set ref :tab 'all))
                                     (eq (plist-get state :tab) 'all))
			(ths--button 'favorites "自选" (lambda () (ths--set ref :tab 'favorites))
                                     (eq (plist-get state :tab) 'favorites))
			(ths--button 'sort "涨幅 ↓" (lambda () (ths--set ref :sorted (not (plist-get (etaf-value ref) :sorted))))
                                     (plist-get state :sorted))))
       (ths--text "  代码    名称        最新     涨跌幅" (ths-theme-color 'muted)))
      (mapcar
       (lambda (stock)
	 (let* ((code (ths-data-id stock))
		(selected (equal code (plist-get state :selected)))
		(label (format "%s %s  %s %8s  %8s"
                               (if (member code (plist-get state :favorites)) "*" " ")
                               code (truncate-string-to-width (plist-get stock :name) 8 0 ?\s)
                               (ths-data-format-price stock (plist-get stock :price))
                               (ths--number (ths-data-change stock) "%+.2f%%"))))
           (etaf-node 'box
                      (list :ref (intern (concat "stock-" code)) :key code
                            :role 'button :tab-index 0 :height (list 'lh row-height) :overflow 'hidden
                            ;; Vertical layout uses whole text rows.  Explicit,
                            ;; symmetric padding centers the single-line label.
                            :padding (list (list 'lh (if ths--roomy 1 0)) '(ch 0))
                            :on-press (lambda () (ths--set ref :selected code) (ths--set ref :section 'detail))
                            :bgcolor (if selected (ths-theme-color 'selected) (ths-theme-color 'panel))
                            :help-echo (concat "查看 " (plist-get stock :name)))
                      (list (ths--text label (ths--color (ths-data-change stock)))))))
       visible)
      (list (ths--space free))
      (list (etaf-node 'row '(:height (lh 1) :item-gap (ch 1))
                       (list (ths--button 'stock-prev "上页 p" (lambda () (ths-stock-page t)))
                             (ths--text (format "%d/%d" (1+ page) (max 1 (ceiling (length stocks) capacity))))
                             (ths--button 'stock-next "下页 n" #'ths-stock-page))))
      (unless (ths--stocks state)
	(list (ths--text (if (and (eq (plist-get state :tab) 'favorites)
				  (null (plist-get state :favorites)))
                             "暂无自选；按 a 添加股票或基金。"
                           "没有匹配股票；按 / 清空搜索。") (ths-theme-color 'warning)))))
     42 'watchlist-panel)))

(defun ths--detail (state ref width)
  "Build the appropriate detail view for the selected security."
  (if (ths-data-otc-p (ths-data-find (plist-get state :selected)))
      (ths--fund-detail state ref width)
    (ths--exchange-detail state ref width)))

(defun ths--fund-detail (state ref width)
  "Display published fund values within STATE's panel and WIDTH."
  (let* ((stock (ths-data-find (plist-get state :selected)))
         (money (plist-get stock :money-fund))
         (accumulated (eq (plist-get state :chart) 'daily))
         (unit-label (if money "万份收益" "单位净值"))
         (acc-label (if money "七日年化" "累计净值"))
         (rows (ths-chart-nav-rows stock accumulated))
         (chart-rows (max 2 (- ths--panel-rows (ths--panel-overhead) 7)))
         (chart-width (max 2 (- width 4)))
         (change (ths-data-change stock)))
    (ths--panel
     (format "%s  %s / 场外" (plist-get stock :name) (plist-get stock :code))
     (list
      (ths--text (format "%s %s%s" unit-label (ths-data-format-price stock (plist-get stock :price))
                         (if money " 元" (concat "  日增长 " (ths--number change "%+.2f%%"))))
                 (if money (ths-theme-color 'foreground) (ths--color change)))
      (ths--text (format "%s %s%s" acc-label (ths-data-format-price stock (plist-get stock :acc-nav))
                         (if money "%" "")) (ths-theme-color 'secondary))
      (ths--text (format "公布日期 %s" (or (plist-get stock :time) "--")) (ths-theme-color 'secondary))
      (etaf-node 'row '(:item-gap (ch 1))
                 (list (ths--button 'intraday unit-label (lambda () (ths--set ref :chart 'intraday)) (not accumulated))
                       (ths--button 'daily acc-label (lambda () (ths--set ref :chart 'daily)) accumulated)))
      (ths--text (format "%s走势 · 最近 %d 期" (if accumulated acc-label unit-label) (length rows))
                 (ths-theme-color 'secondary))
      (etaf-node
       'column (list :ref 'chart-region :height (list 'lh chart-rows))
       (list
        (cond
         ((not rows) (ths--text (if (plist-get stock :quote-error) "暂无净值数据 · 获取失败，按 r 重试" "暂无净值数据 · 等待公布")
                                (ths-theme-color 'warning)))
         ((and (display-graphic-p) (image-type-available-p 'svg))
          (etaf-node 'image
                     (list :ref 'nav-chart :src (list 'image :type 'svg :data
                                                       (ths-chart-nav stock accumulated chart-width chart-rows t))
                           :columns chart-width :rows chart-rows :adapter 'auto) nil))
         (t (ths--text (ths-chart-nav stock accumulated (max 2 (- chart-width 10)) chart-rows))))))
      (ths--text (if rows (format "%s — %s" (plist-get (car rows) :date) (plist-get (car (last rows)) :date)) "--")
                 (ths-theme-color 'muted))
      (ths--text (concat "天天基金 · 公布数据"
                         (if (plist-get stock :quote-error) " · 更新失败，保留上次数据" ""))
                 (ths-theme-color 'secondary)))
     width 'detail-panel)))

(defun ths--fund-info (state)
  "Build fund metadata instead of an order book for STATE."
  (let* ((stock (ths-data-find (plist-get state :selected)))
         (money (plist-get stock :money-fund)))
    (ths--panel
     "基金信息"
     (list (ths--text (plist-get stock :symbol))
           (ths--text (if money "货币基金 · 公布收益" "场外基金 · 公布净值"))
           (ths--text (format "净值日期 %s" (or (plist-get stock :time) "--")))
           (ths--text "数据源：天天基金")
           (ths--text "每日公布，非盘中估值")
           (ths--text (if money "万份收益：元 / 万份" "日增长率采用接口公布值"))
           (ths--text (if money "七日年化：百分比" "累计净值包含历史分红"))
           (ths--text "无场内盘口或成交记录"))
     32 'book-panel)))

(defun ths--fund-history (state)
  "Show the most recent published fund values within STATE's height budget."
  (let* ((stock (ths-data-find (plist-get state :selected)))
         (money (plist-get stock :money-fund))
         (rows (seq-take (reverse (plist-get stock :nav-history))
                         (max 0 (- ths--panel-rows (ths--panel-overhead) 2)))))
    (ths--panel
     (if money "历史收益" "历史净值")
     (append
      (list (ths--text (if money "日期        万份收益  七日年化" "日期        单位净值  日增长率") (ths-theme-color 'muted)))
      (mapcar (lambda (row)
                (ths--text (format "%s %8s %7s" (plist-get row :date)
                                   (ths-data-format-price stock (plist-get row :nav))
                                   (ths--number (plist-get row (if money :acc-nav :change)) "%.2f%%"))
                           (if money (ths-theme-color 'foreground) (ths--color (plist-get row :change))))) rows)
      (list (ths--text "最近公布记录在上" (ths-theme-color 'muted))))
     32 'trades-panel)))

(defun ths--book (state)
  "Choose fund information or the exchange order book for STATE."
  (if (ths-data-otc-p (ths-data-find (plist-get state :selected)))
      (ths--fund-info state)
    (ths--exchange-book state)))

(defun ths--trades (state)
  "Choose fund history or recent exchange trades for STATE."
  (if (ths-data-otc-p (ths-data-find (plist-get state :selected)))
      (ths--fund-history state)
    (ths--exchange-trades state)))

(defun ths--exchange-detail (state ref width)
  "Build quote and chart panel from STATE and REF within WIDTH cells."
  (let ((stock (ths-data-find (plist-get state :selected)))
        (chart-width (- width 2))
        (chart-rows (max 4 (- ths--panel-rows (ths--panel-overhead) 7 (if ths--roomy 2 0)))))
    (if (not stock)
        (ths--panel "个股详情" (list (ths--text "请从行情列表选择股票。")) width 'detail-panel)
      (let* ((change (ths-data-change stock))
             (intraday (eq (plist-get state :chart) 'intraday))
             (available (if intraday (and (plist-get stock :points) (ths-data-chart-base stock))
                          (plist-get stock :candles)))
             (kind (if intraday 'intraday 'daily))
             (chart-error (plist-get stock (intern (format ":%s-error" kind)))))
        (ths--panel
         (format "%s  %s  /  %s" (plist-get stock :name) (ths-data-id stock)
                 (plist-get stock :sector))
         (list
          (etaf-node 'row '(:item-gap (ch 2))
                     (list (ths--text (format "%s  %s  %s" (ths-data-format-price stock (plist-get stock :price))
                                              (ths-data-format-price stock (when change (- (plist-get stock :price) (plist-get stock :close))) t)
                                              (ths--number change "%+.2f%%"))
                                      (ths--color change))
                           (ths--button 'favorite-toggle
                                        (if (member (ths-data-id stock) (plist-get state :favorites)) "移出自选" "+ 加自选")
                                        (lambda () (ths--favorite ref)))))
          (ths--text (format "昨收 %s    最高 %s    最低 %s"
                             (ths-data-format-price stock (plist-get stock :close))
                             (ths-data-format-price stock (plist-get stock :high))
                             (ths-data-format-price stock (plist-get stock :low))) (ths-theme-color 'secondary))
          (ths--text (format "成交额 %s    换手率 %s" (ths--amount (plist-get stock :amount)) (ths--number (plist-get stock :turnover) "%.2f%%")) (ths-theme-color 'secondary))
          (ths--space (if ths--roomy 1 0))
          (etaf-node 'row '(:item-gap (ch 1))
                     (list (ths--button 'intraday "分时" (lambda () (ths--set ref :chart 'intraday)) intraday)
                           (ths--button 'daily "日 K" (lambda () (ths--set ref :chart 'daily)) (not intraday))))
          (ths--text (if intraday "分时走势  · 蓝色价格 / 黄色均线" "日 K  · 红涨绿跌 / 不复权") (ths-theme-color 'secondary))
          (etaf-node 'column (list :ref 'chart-region :height (list 'lh chart-rows))
                     (list (cond
                            ((not available) (ths--text (if chart-error "暂无图表数据 · 获取失败，按 r 重试" "暂无图表数据 · 等待获取") (ths-theme-color 'warning)))
                            ((and (display-graphic-p) (image-type-available-p 'svg))
                             (etaf-node 'image (list :ref (if intraday 'intraday-chart 'daily-chart)
                                                     :src (list 'image :type 'svg :data
                                                                (if intraday (ths-chart-svg stock)
                                                                  (ths-chart-candles-svg stock (max 1 (/ (- chart-width 10) 2)))))
                                                     :columns chart-width :rows chart-rows :adapter 'auto) nil))
                            (t (etaf-node 'column nil
                                          (list (ths--text (if intraday (ths-chart-line stock (- chart-width 10) (1- chart-rows))
                                                             (ths-chart-candles stock (/ (- chart-width 10) 2) (1- chart-rows))))
                                                (ths--text (if intraday "         09:30               11:30 / 13:00          15:00"
                                                             (let ((candles (ths-data-candles stock (/ (- chart-width 10) 2))))
                                                               (format "         %s — %s" (plist-get (car candles) :date)
                                                                       (plist-get (car (last candles)) :date)))) (ths-theme-color 'muted))))))))
          (ths--space (if ths--roomy 1 0))
          (ths--text (format "报价 %s · %s%s" (ths--source (plist-get stock :quote-source))
                             (or (plist-get stock :time) "暂无行情时间")
                             (if (plist-get stock :quote-error) " · 更新失败，保留上次数据" "")) (ths-theme-color 'secondary))
          (ths--text (format "图表 %s · %s%s"
                             (ths--source (plist-get stock (intern (format ":%s-source" kind))))
                             (if intraday (or (plist-get stock :intraday-date) "--")
                               (or (plist-get (car (last (plist-get stock :candles))) :date) "--"))
                             (if chart-error " · 更新失败，保留上次数据" "")) (ths-theme-color 'secondary)))
         width 'detail-panel)))))

(defun ths--exchange-book (state)
  "Build the five-level book for STATE."
  (let ((stock (ths-data-find (plist-get state :selected))))
    (ths--panel
     "五档盘口 / 手"
     (if (not stock) (list (ths--text "暂无选中股票"))
       (append
        (list (ths--text "档位    价格     数量" (ths-theme-color 'muted)))
        (cl-loop for (label price volume side) in (ths-data-book stock)
                 append (append
                         (when (and ths--roomy (equal label "买1"))
                           ;; Box-drawing glyphs can use a wide fallback font.
                           ;; ASCII fits within the panel's content area.
                           (list (ths--text (make-string 20 ?-) (ths-theme-color 'border))))
                         (list (ths--text (format "%s %9s %7s" label (ths-data-format-price stock price) (ths--number volume "%.2f"))
                                          (if (eq side 'ask) (ths-theme-color 'up) (ths-theme-color 'down))))))
        (list (ths--space (if ths--roomy 1 0))
              (ths--text (if (ths-data-book stock)
                             (format "%s · %s" (ths--source (plist-get stock :quote-source))
                                     (if (plist-get stock :quote-error) "更新失败" "行情快照"))
                           "暂无盘口数据") (ths-theme-color 'secondary)))))
     32 'book-panel)))

(defun ths--trade-cells (&optional trade stock)
  "Build cells for TRADE in STOCK's price precision, or headings when nil."
  (let ((side (plist-get trade :side)))
    (mapcar
     (lambda (field)
       (let* ((key (car field)) (width (nth 2 field))
              (value (plist-get trade key))
              (text (if (not trade) (nth 1 field)
                      (pcase key
                        (:price (ths-data-format-price stock value))
                        (:volume (if (= value (truncate value)) (format "%.0f" value) (format "%.2f" value)))
                        (:side (pcase value ('buy "买盘") ('sell "卖盘") ('neutral "中性") (_ "未知")))
                        (_ value))))
              (color (pcase side ('buy (ths-theme-color 'up)) ('sell (ths-theme-color 'down)) (_ (ths-theme-color 'secondary)))))
         (etaf-node 'box (list :width (list 'ch width) :height '(lh 1) :overflow 'hidden :help-echo text)
                    (list (ths--text text color)))))
     '((:time "时间" 9) (:price "价格" 8) (:volume "量/手" 8) (:side "性质" 5)))))

(defun ths--exchange-trades (state)
  "Build recent trades using the full assigned panel height for STATE."
  (let* ((stock (ths-data-find (plist-get state :selected)))
         (stamp (plist-get stock :trades-fetched-at))
         (failure (plist-get stock :trades-error))
         (available (- ths--panel-rows (ths--panel-overhead) 5
                       (if ths--roomy 1 0) (if failure 1 0)))
         (trades (seq-take (plist-get stock :trades) (max 1 available)))
         (table-rows (if trades (min available (* (if ths--roomy 2 1) (length trades))) 1))
         (free (- available table-rows)))
    (ths--panel
     "成交记录 / 分时成交"
     (append
      (list (etaf-node 'row nil (ths--trade-cells)))
      (cl-loop for trade in trades for i from 0
               for row-height = (- (/ (* (1+ i) table-rows) (length trades))
                                   (/ (* i table-rows) (length trades)))
               collect (etaf-node 'row (list :height (list 'lh row-height)) (ths--trade-cells trade stock)))
      (unless trades (list (ths--text (cond (failure "获取失败，按 r 重试")
                                            (stamp "暂无成交记录")
                                            (t "等待成交记录…")) (ths-theme-color 'warning))))
      (list (ths--space (+ (max 0 free) (if ths--roomy 1 0)))
            (ths--text "东方财富 · 最新记录在上" (ths-theme-color 'secondary))
            (ths--text (if stamp (concat "采集 " (format-time-string "%m-%d %T" (seconds-to-time stamp) 28800))
                         "尚未获取数据") (ths-theme-color 'muted))
            (ths--text "接口未提供交易日期" (ths-theme-color 'muted))
            (ths--text "分时汇总 · 非完整逐笔" (ths-theme-color 'muted)))
      (when failure
        (list (etaf-node 'box (list :help-echo failure)
                         (list (ths--text (if trades "更新失败，保留上次记录" "成交接口暂不可用") (ths-theme-color 'warning)))))))
     32 'trades-panel)))

(defun ths--market-sidebar (state)
  "Stack the order book and recent trades for STATE."
  (etaf-node 'column '(:width (ch 32) :item-gap (lh 1))
             (list (let ((ths--panel-rows (ths--book-height))) (ths--book state))
                   (let ((ths--panel-rows (- ths--panel-rows (ths--book-height) 1))) (ths--trades state)))))

(defun ths--section (section)
  "Show SECTION in a compact window."
  (ths--set ths--state :section section))

(defun ths--pin-window-top (buffer)
  "Keep windows showing BUFFER at the top after resizing or rendering."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (set-window-start window (with-current-buffer buffer (point-min)) t)
      (set-window-vscroll window 0))))

(defun ths--shortcut-command (key)
  "Run KEY's command, restoring the ranked stock before toolbar navigation."
  (when (and (eq (plist-get (etaf-value ths--state) :page) 'ranking)
             (member key '("j" "k" "RET"))
             (equal (car ths--ranking-toolbar-stock) (ths--ranking-key (etaf-value ths--state))))
    (let* ((rows (plist-get (ths--ranking-data (etaf-value ths--state)) :rows))
           (index (cl-position (plist-get (cdr ths--ranking-toolbar-stock) :symbol) rows
                               :test #'equal :key (lambda (stock) (plist-get stock :symbol)))))
      (when index (ths--ranking-focus-row rows index))))
  (if (equal key "RET")
      (if (eq (plist-get (etaf-value ths--state) :page) 'watchlist)
          (when-let* ((stock (ths-data-find (plist-get (etaf-value ths--state) :selected))))
            (ths--watchlist-open ths--state stock))
        (let* ((runtime (etaf-runtime-for-buffer (current-buffer)))
             (focused (etaf-focused-host-ref runtime)))
        (when (and focused (string-prefix-p "rank-" (symbol-name focused)))
          (etaf-activate runtime))))
    (call-interactively (lookup-key ths-mode-map (kbd key)))))

(defun ths--shortcut-hint (columns &optional ranking state)
  "Build clickable shortcut buttons suited to COLUMNS, RANKING and STATE."
  (let* ((stock (ths-data-find (plist-get state :selected)))
         (watchlist (eq (plist-get state :page) 'watchlist))
         (items (append '(("j" "下一只") ("k" "上一只"))
                        (when watchlist '(("RET" "打开") ("C" "表头")))
                        (if ranking '(("RET" "打开") ("u" "类别") ("s" "切换排行"))
                          '(("a" "添加") ("d" "删除") ("f" "自选") ("s" "排序")))
                        '(("p" "上一页") ("n" "下一页"))
                        (if watchlist '(("/" "搜索"))
                          (if ranking '(("0" "行情"))
                          (if (ths-data-otc-p stock)
                              (if (plist-get stock :money-fund)
                                  '(("1" "收益") ("2" "年化") ("/" "搜索"))
                                '(("1" "单位") ("2" "累计") ("/" "搜索")))
                            '(("1" "分时") ("2" "日K") ("/" "搜索")))))
                        '(("r" "刷新") ("T" "主题") ("q" "退出"))))
         (full-width (+ (1- (length items))
                        (cl-loop for (key label) in items sum (+ 3 (string-width key) (string-width label)))))
         (compact (> full-width (- columns 2))))
    (when (< columns 80)
      (setq items (seq-filter (lambda (item) (member (car item) (cond (ranking '("j" "k" "RET" "u" "s" "r" "T"))
                                                                  (watchlist '("j" "k" "RET" "C" "a" "r"))
                                                                  (t '("j" "k" "a" "d" "/" "r" "T"))))) items)))
    (etaf-node
     'row (list :ref 'shortcut-hint :height '(lh 1) :overflow 'hidden :item-gap '(ch 1))
     (mapcar (lambda (item)
               (let* ((key (car item)) (label (format "%s %s" (cadr item) key)))
                 (ths--button (intern (concat "shortcut-" key))
                              (if compact (format "%s %s" (substring (cadr item) 0 1) key) label)
                              (lambda () (ths--shortcut-command key)) nil label compact))) items))))

(defun ths--dashboard (state ref columns rows)
  "Fit STATE and REF in COLUMNS by ROWS, above the window mode line."
  (let* ((height (max 1 (1- rows)))
         (gap (if (>= rows 36) 1 0))
         (ths--panel-rows (max 1 (- height 3 gap)))
         (ths--roomy (and (not (eq (plist-get state :page) 'ranking)) (>= ths--panel-rows 30)))
         (wide (>= columns 136))
         (section (or (plist-get state :section) (if wide 'detail ths-default-section))))
    (setq ths--stock-page-size
          (max 1 (/ (- ths--panel-rows (ths--panel-overhead) 3) (if ths--roomy 3 1))))
    (cond
     ((or (< columns 44) (< rows 20))
      (etaf-node 'column (list :height (list 'lh height) :overflow 'hidden)
                 (list (ths--text "窗口太小，请放大窗口或缩小字体。"))))
     ((eq (plist-get state :page) 'ranking)
      (ths--ranking-view state ref columns))
     ((eq (plist-get state :page) 'watchlist)
      (ths--watchlist-view state ref columns rows))
     (t
      (etaf-node
       'column (list :ref 'dashboard-frame :height (list 'lh height) :overflow 'hidden
                     :item-gap (list 'lh gap)
                     :bgcolor (ths-theme-color 'background) :color (ths-theme-color 'foreground) :padding '((lh 0) (ch 1)))
       (list
        (etaf-node
         'column '(:ref dashboard-header :height (lh 3))
         (list
          (etaf-node 'row '(:height (lh 1) :overflow hidden :item-gap (ch 1))
                     (list (ths--button 'page-quotes "行情 0" #'ths-show-quotes t)
                           (ths--button 'page-watchlist "自选股 4" #'ths-show-watchlist)
                           (ths--button 'page-ranking "排行榜 3" #'ths-show-ranking)
                           (ths--button 'clear-search "清空" (lambda () (ths--set ref :query "")))))
          (ths--shortcut-hint columns nil state)
          (etaf-node
           'row '(:height (lh 1) :overflow hidden :item-gap (ch 3))
           (mapcar
            (lambda (stock)
              (let ((change (ths-data-change stock)))
                (etaf-node
                 'row '(:height (lh 1) :item-gap (ch 1))
                 (list (ths--text (plist-get stock :name) (ths-theme-color 'secondary))
                       (ths--text (format "%s %s" (ths--number (plist-get stock :price))
                                          (ths--number change "%+.2f%%"))
                                  (ths--color change))))))
            ths-data-indices))))
        (etaf-node
         'row (list :ref 'dashboard-body :height (list 'lh ths--panel-rows) :item-gap '(ch 1))
         (if wide
             (list (ths--watchlist state ref)
                   (ths--detail state ref (- columns 78))
                   (if (>= ths--panel-rows (+ (ths--book-height) 1 (ths--panel-overhead) 8))
                       (ths--market-sidebar state)
                     (if (eq section 'trades) (ths--trades state) (ths--book state))))
           (list (pcase section
                   ('list (ths--watchlist state ref))
                   ('book (ths--book state))
                   ('trades (ths--trades state))
                   (_ (ths--detail state ref (- columns 2)))))))))))))

(etaf-define-component ths-dashboard (&key state)
                       "Reactive market dashboard."
                       :setup
                       (let ((buffer (current-buffer)))
                         (etaf-theme-provide
                          (etaf-computed
                           (lambda ()
                             ;; Revision changes also refresh static focus styles.
                             (etaf-value state)
                             (list :ths-panel (ths-theme-color 'panel)
                                   :ths-alternate (ths-theme-color 'alternate)
                                   :ths-selected (ths-theme-color 'selected)))))
                         (etaf-on-updated (lambda () (ths--pin-window-top buffer)))
                         (etaf-on-unmounted (lambda () (when (buffer-live-p buffer)
                                                         (with-current-buffer buffer (ths--stop)))))
                         nil)
                       :view
                       (column
                        (expr
                         (let ((metrics (etaf-display-metrics))
                               (viewport (etaf-runtime-viewport)))
                           ;; Use the same font and pixel budget as the renderer.  Reading both
                           ;; sources also rebuilds image slices after buffer text scaling.
                           (ths--dashboard (etaf-value state) state
                                           (floor (/ (float (car viewport))
                                                     (plist-get metrics :char-width)))
                                           (or (cdr viewport) (window-body-height) 40)))))
                       :styles ((".ranking-row" :bgcolor (etaf-theme-token :ths-panel))
                                (".ranking-row-alt" :bgcolor (etaf-theme-token :ths-alternate))
                                (".ranking-row:focus" :bgcolor (etaf-theme-token :ths-selected))))

(defun ths-next-stock (&optional backwards)
  "Select the next visible stock, or previous if BACKWARDS is non-nil."
  (interactive)
  (if (eq (plist-get (etaf-value ths--state) :page) 'ranking)
      (ths-ranking-next-stock backwards)
    (let* ((state (etaf-value ths--state)) (stocks (ths--stocks state))
         (codes (mapcar (lambda (stock) (ths-data-id stock)) stocks)))
    (when codes
      (ths--set ths--state :selected
                (nth (mod (+ (or (cl-position (plist-get state :selected) codes :test #'equal) 0)
                             (if backwards -1 1)) (length codes)) codes))))))

(defun ths-previous-stock () "Select the previous stock." (interactive) (ths-next-stock t))
(defun ths-search () "Search stocks by code, name or sector." (interactive) (ths--search ths--state))
(defun ths-toggle-favorite () "Toggle selected stock in the watchlist." (interactive) (ths--favorite ths--state))
(defun ths-sort () "Toggle quote sorting or cycle through ranking metrics." (interactive)
       (let ((state (etaf-value ths--state)))
	 (if (eq (plist-get state :page) 'ranking)
             (ths--ranking-select ths--state (if (eq (plist-get state :ranking-sort) 'change) 'amount 'change))
	   (ths--set ths--state :sorted (not (plist-get state :sorted))))))
(defun ths-intraday () "Show the intraday chart." (interactive) (ths--set ths--state :chart 'intraday))
(defun ths-daily () "Show daily candles." (interactive) (ths--set ths--state :chart 'daily))
(defun ths-refit ()
  "Fit the layout to the current window width."
  (interactive)
  (etaf-refit)
  (ths--pin-window-top (current-buffer)))

(defvar ths-mode-map (make-sparse-keymap)
  "Additional keys, layered over ETAF's native focus and activation map.")
;; Reinstall bindings when reloading an older checkout in-place.
(dolist (key '("l" "c" "b" "t"))
  (define-key ths-mode-map (kbd key) nil))
(dolist (binding '(("j" . ths-next-stock) ("k" . ths-previous-stock)
                   ("/" . ths-search) ("f" . ths-toggle-favorite) ("s" . ths-sort)
                   ("a" . ths-add-favorite) ("d" . ths-remove-favorite)
                   ("1" . ths-intraday) ("2" . ths-daily) ("g" . ths-refit)
                   ("0" . ths-show-quotes) ("3" . ths-show-ranking)
                   ("4" . ths-show-watchlist) ("C" . ths-configure-watchlist-columns)
                   ("u" . ths-ranking-cycle-universe)
                   ("n" . ths-next-page) ("p" . ths-previous-page)
                   ("r" . ths-refresh) ("T" . ths-toggle-theme) ("q" . quit-window)))
  (define-key ths-mode-map (kbd (car binding)) (cdr binding)))

(define-minor-mode ths-mode
  "Keyboard commands for the THS dashboard."
  :lighter " THS" :keymap ths-mode-map)

;;;###autoload
(defun ths ()
  "Open the Tonghuashun-style ETAF dashboard with live market providers."
  (interactive)
  (let ((buffer (get-buffer-create ths-buffer-name)))
    (unless (etaf-runtime-for-buffer buffer)
      (with-current-buffer buffer
        (let ((favorites (ths-watchlist-load)))
          (setq ths-data-stocks (mapcar #'ths-data-placeholder (delete-dups (append ths-data-symbols favorites)))
		ths-data-indices (mapcar #'ths-data-placeholder ths-data-index-symbols)
		ths--rankings (make-hash-table :test #'equal)
                ths--ranking-positions (make-hash-table :test #'equal))
          (let ((state (etaf-ref (ths--initial-state favorites))))
            (setq-local truncate-lines t)
            (ths--apply-buffer-theme)
            (etaf-mount buffer (etaf-view (ths-dashboard :state state)))
            (setq-local ths--state state)
            (setq ths--session (list 'ths-session) ths--requests (make-hash-table :test #'equal))
            (add-hook 'kill-buffer-hook #'ths--stop nil t)
            (read-only-mode 1)
            (ths-mode 1)
            (when ths-auto-fetch
              (ths-refresh)
              (when ths-refresh-interval
		(setq ths--timer
                      (run-at-time (max 5 ths-refresh-interval) (max 5 ths-refresh-interval)
                                   (lambda () (when (buffer-live-p buffer)
						(with-current-buffer buffer (ths-refresh t))))))))))))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer-same-window buffer)
      (ths-refit))
    (with-current-buffer buffer
      (when (and (fboundp 'meow-motion-mode) (not (bound-and-true-p meow-motion-mode)))
        (meow-motion-mode 1)))
    buffer))

(provide 'ths)
;;; ths.el ends here
