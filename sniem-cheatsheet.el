;;; sniem-cheatsheet.el --- Simple united editing method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs))
;; Homepage: https://github.com/SpringHan/sniem.git
;; Keywords: Editing Method


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Simple united editing method.

;;; Code:

(defconst sniem-cheatsheet-layout-qwerty
  "                                                      Cheatsheet For Qwerty

┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━━━━━┓
┃       ~ │       ! │       @ │       # │       $ │       % │       ^ │       & │       * │       ( │       ) │       _ │       + │         DEL ┃
┃      [~]|      [!]|      [@]|      [#]|      [$]|      [%]|      [^]|      [&]|      [*]|      [(]|      [)]|      [_]|      [+]|             ┃
┠─┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤        [DEL]┃
┃       ` │       1 │       2 │       3 │       4 │       5 │       6 │       7 │       8 │       9 │       0 │       - │       = │             ┃
┃      [`]|      [1]|      [2]|      [3]|      [4]|      [5]|      [6]|      [7]|      [8]|      [9]|      [0]|      [-]|      [=]|             ┃
┠─────────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────────┨
┃         TAB │       Q │       W │       E │       R │       T │       Y │       U │       I │       O │       P │       { │       } │       | ┃
┃             |      [Q]|      [W]|      [E]|      [R]|      [T]|      [Y]|      [U]|      [I]|      [O]|      [P]|      [{]|      [}]|      [|]┃
┃             ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┨
┃             │       q │       w │       e │       r │       t │       y │       u │       i │       o │       p │       [ │       ] │       \\ ┃
┃             |      [q]|      [w]|      [e]|      [r]|      [t]|      [y]|      [u]|      [i]|      [o]|      [p]|      [[]|      []]|      [\\]┃
┠─────────────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─────────┨
┃               │       A │       S │       D │       F │       G │       H │       J │       K │       L │       : │       \" │             RET ┃
┃               |      [A]|      [S]|      [D]|      [F]|      [G]|      [H]|      [J]|      [K]|      [L]|      [:]|      [\"]│                 ┃
┃               ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤            [RET]┃
┃               │       a │       s │       d │       f │       g │       h │       j │       k │       l │       ; │       ' │                 ┃
┃               |      [a]|      [s]|      [d]|      [f]|      [g]|      [h]|      [j]|      [k]|      [l]|      [;]|      [']|                 ┃
┠───────────────┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────────────────┨
┃                     │       Z │       X │       C │       V │       B │       N │       M │       < │       > │       ? │                     ┃
┃                     |      [Z]|      [X]|      [C]|      [V]|      [B]|      [N]|      [M]|      [<]|      [>]|      [?]|                     ┃
┃                     ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤                     ┃
┃                     │       z │       x │       c │       v │       b │       n │       m │       , │       . │       / │                     ┃
┃                     |      [z]|      [x]|      [c]|      [v]|      [b]|      [n]|      [m]|      [,]|      [.]|      [/]|                     ┃
┗━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━╅─────────┴─────────┴─────────┴─────────╆━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━┛
                                                    ┃                                   SPC ┃
                                                    ┃                                  [SPC]┃
                                                    ┃                                       ┃
                                                    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
" )

(defconst sniem-cheatsheet-layout-dvp
  "                                                 Cheatsheet For Programmer Dvorak

┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━━━━━┓
┃       ~ │       % │       7 │       5 │       3 │       1 │       9 │       0 │       2 │       4 │       6 │       8 │       ` │         DEL ┃
┃      [~]│      [%]│      [7]│      [5]│      [3]│      [1]│      [9]│      [0]│      [2]│      [4]│      [6]│      [8]│      [`]│             ┃
┠─┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤        [DEL]┃
┃       $ │       & │       [ │       { │       } │       ( │       = │       * │       ) │       + │       ] │       ! │       # │             ┃
┃      [$]│      [&]│      [[]│      [{]│      [}]│      [(]│      [=]│      [*]│      [)]│      [+]│      []]│      [!]│      [#]│             ┃
┠─────────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────────┨
┃         TAB │       : │       < │       > │       P │       Y │       F │       G │       C │       R │       L │       ? │       ^ │       | ┃
┃             │      [:]│      [<]│      [>]│      [P]│      [Y]│      [F]│      [G]│      [C]│      [R]│      [L]│      [?]│      [^]│      [|]┃
┃             ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┨
┃             │       ; │       , │       . │       p │       y │       f │       g │       c │       r │       l │       / │       @ │       \\ ┃
┃             │      [;]│      [,]│      [.]│      [p]│      [y]│      [f]│      [g]│      [c]│      [r]│      [l]│      [/]│      [@]│      [\\]┃
┠─────────────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─────────┨
┃               │       A │       O │       E │       U │       I │       D │       H │       T │       N │       S │       _ │             RET ┃
┃               │      [A]│      [O]│      [E]│      [U]│      [I]│      [D]│      [H]│      [T]│      [N]│      [S]│      [_]│                 ┃
┃               ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤            [RET]┃
┃               │       a │       o │       e │       u │       i │       d │       h │       t │       n │       s │       - │                 ┃
┃               │      [a]│      [o]│      [e]│      [u]│      [i]│      [d]│      [h]│      [t]│      [n]│      [s]│      [-]│                 ┃
┠───────────────┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────────────────┨
┃                     │       \" │       Q │       J │       K │       X │       B │       M │       W │       V │       Z │                     ┃
┃                     │      [\"]│      [Q]│      [J]│      [K]│      [X]│      [B]│      [M]│      [W]│      [V]│      [Z]│                     ┃
┃                     ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤                     ┃
┃                     │       ' │       q │       j │       k │       x │       b │       m │       w │       v │       z │                     ┃
┃                     │      [']│      [q]│      [j]│      [k]│      [x]│      [b]│      [m]│      [w]│      [v]│      [z]│                     ┃
┗━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━╅─────────┴─────────┴─────────┴─────────╆━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━┛
                                                    ┃                                   SPC ┃
                                                    ┃                                  [SPC]┃
                                                    ┃                                       ┃
                                                    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
")

(defconst sniem-cheatsheet-layout-dvorak
  "                                                 Cheatsheet For Dvorak Simplified

┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━━━━━┓
┃       ~ │       ! │       @ │       # │       $ │       % │       ^ │       & │       * │       ( │       ) │       { │       } │         DEL ┃
┃      [~]|      [!]|      [@]|      [#]|      [$]|      [%]|      [^]|      [&]|      [*]|      [(]|      [)]|      [{]|      [}]|             ┃
┠─┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤        [DEL]┃
┃       ` │       1 │       2 │       3 │       4 │       5 │       6 │       7 │       8 │       9 │       0 │       [ │       ] │             ┃
┃      [`]|      [1]|      [2]|      [3]|      [4]|      [5]|      [6]|      [7]|      [8]|      [9]|      [0]|      [[]|      []]|             ┃
┠─────────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────────┨
┃         TAB │       \" │       < │       > │       P │       Y │       F │       G │       C │       R │       L │       ? │       + │       | ┃
┃             │      [\"]│      [<]│      [>]│      [P]│      [Y]│      [F]│      [G]│      [C]│      [R]│      [L]│      [?]│      [+]│      [|]┃
┃             ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┨
┃             │       ' │       , │       . │       p │       y │       f │       g │       c │       r │       l │       / │       = │       \\ ┃
┃             │      [']│      [,]│      [.]│      [p]│      [y]│      [f]│      [g]│      [c]│      [r]│      [l]│      [/]│      [=]│      [\\]┃
┠─────────────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─────────┨
┃               │       A │       O │       E │       U │       I │       D │       H │       T │       N │       S │       _ │             RET ┃
┃               │      [A]│      [O]│      [E]│      [U]│      [I]│      [D]│      [H]│      [T]│      [N]│      [S]│      [_]│                 ┃
┃               ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤            [RET]┃
┃               │       a │       o │       e │       u │       i │       d │       h │       t │       n │       s │       - │                 ┃
┃               │      [a]│      [o]│      [e]│      [u]│      [i]│      [d]│      [h]│      [t]│      [n]│      [s]│      [-]│                 ┃
┠───────────────┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────────────────┨
┃                     │       : │       Q │       J │       K │       X │       B │       M │       W │       V │       Z │                     ┃
┃                     │      [:]│      [Q]│      [J]│      [K]│      [X]│      [B]│      [M]│      [W]│      [V]│      [Z]│                     ┃
┃                     ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤                     ┃
┃                     │       ; │       q │       j │       k │       x │       b │       m │       w │       v │       z │                     ┃
┃                     │      [;]│      [q]│      [j]│      [k]│      [x]│      [b]│      [m]│      [w]│      [v]│      [z]│                     ┃
┗━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━╅─────────┴─────────┴─────────┴─────────╆━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━┛
                                                    ┃                                   SPC ┃
                                                    ┃                                  [SPC]┃
                                                    ┃                                       ┃
                                                    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
")

(defconst sniem-cheatsheet-layout-colemak
  "                                                      Cheatsheet For Colemak

┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━━━━━┓
┃       ~ │       ! │       @ │       # │       $ │       % │       ^ │       & │       * │       ( │       ) │       _ │       + │         DEL ┃
┃      [~]|      [!]|      [@]|      [#]|      [$]|      [%]|      [^]|      [&]|      [*]|      [(]|      [)]|      [_]|      [+]|             ┃
┠─┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤        [DEL]┃
┃       ` │       1 │       2 │       3 │       4 │       5 │       6 │       7 │       8 │       9 │       0 │       - │       = │             ┃
┃      [`]|      [1]|      [2]|      [3]|      [4]|      [5]|      [6]|      [7]|      [8]|      [9]|      [0]|      [-]|      [=]|             ┃
┠─────────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────┴───┬─────────┨
┃         TAB │       Q │       W │       F │       P │       G │       J │       L │       U │       Y │       : │       { │       } │       | ┃
┃             |      [Q]|      [W]|      [F]|      [P]|      [G]|      [J]|      [L]|      [U]|      [Y]|      [:]|      [{]|      [}]|      [|]┃
┃             ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┨
┃             │       q │       w │       f │       p │       g │       j │       l │       u │       y │       ; │       [ │       ] │       \\ ┃
┃             |      [q]|      [w]|      [f]|      [p]|      [g]|      [j]|      [l]|      [u]|      [y]|      [;]|      [[]|      []]|      [\\]┃
┠─────────────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─┬───────┴─────────┨
┃               │       A │       R │       S │       T │       D │       H │       N │       E │       I │       O │       \" │             RET ┃
┃               |      [A]|      [R]|      [S]|      [T]|      [D]|      [H]|      [N]|      [E]|      [I]|      [O]|      [\"]│                 ┃
┃               ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤            [RET]┃
┃               │       a │       r │       s │       t │       d │       h │       n │       e │       i │       o │       ' │                 ┃
┃               |      [a]|      [r]|      [s]|      [t]|      [d]|      [h]|      [n]|      [e]|      [i]|      [o]|      [']|                 ┃
┠───────────────┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────┬───┴─────────────────┨
┃                     │       Z │       X │       C │       V │       B │       K │       M │       < │       > │       ? │                     ┃
┃                     |      [Z]|      [X]|      [C]|      [V]|      [B]|      [K]|      [M]|      [<]|      [>]|      [?]|                     ┃
┃                     ├┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┤                     ┃
┃                     │       z │       x │       c │       v │       b │       k │       m │       , │       . │       / │                     ┃
┃                     |      [z]|      [x]|      [c]|      [v]|      [b]|      [k]|      [m]|      [,]|      [.]|      [/]|                     ┃
┗━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━╅─────────┴─────────┴─────────┴─────────╆━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━┛
                                                    ┃                                   SPC ┃
                                                    ┃                                  [SPC]┃
                                                    ┃                                       ┃
                                                    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
" )

(defconst sniem-cheatsheet-keys
  '("a" "A" "b" "B" "c" "C" "d" "D" "e" "E" "f" "F" "g" "G" "h" "H" "i" "I" "j" "J"
    "k" "K" "l" "L" "m" "M" "n" "N" "o" "O" "p" "P" "q" "Q" "r" "R" "s" "S" "t" "T"
    "u" "U" "v" "V" "w" "W" "x" "X" "y" "Y" "z" "Z" "0" "1" "2" "3" "4" "5" "6" "7"
    "8" "9" "!" "@" "" "$" "%" "^" "&" "*" "" "~" "-" "_" "=" "+" "," "<" "." ">"
    "/" "?" "(" ")" "[" "]" "{" "}" "\\" "|" ";" ":" "'" "\"" "#" "`" "SPC" "RET" "DEL"))

(defcustom sniem-cheatsheet-special-key-space
  '(("" . "        ")
    ("" . "            ")
    (" " . "                                  "))
  "The space for special key."
  :type 'list
  :group 'sniem)

(defcustom sniem-cheatsheet-special-key-width
  '(("" . 13)
    ("" . 17)
    (" " . 39))
  "The space for special key."
  :type 'list
  :group 'sniem)

(defcustom sniem-cheatsheet-short-commands
  '((sniem-forward-char . "→")
    (sniem-backward-char . "←")
    (sniem-5-forward-char . "5*→")
    (sniem-5-backward-char . "←*5")
    (sniem-next-line . "↓")
    (sniem-prev-line . "↑")
    (sniem-5-next-line . "↓*5")
    (sniem-5-prev-line . "↑*5")
    (digit-argument . "num-arg")
    (sniem-digit-argument-or-fn . "num-arg-middle-keyboard-with-functions")
    (sniem-beginning-of-line . "line-beg")
    (sniem-end-of-line . "line-end")
    (sniem-first-line . "1st line")
    (sniem-goto-line . "goto-line")
    (sniem-next-word . "word →")
    (sniem-prev-word . "word ←")
    (sniem-next-symbol . "symbol→")
    (sniem-prev-symbol . "symbol←")
    (sniem-beg-of-mark . "mark-beg")
    (sniem-end-of-mark . "mark-end")
    (sniem-lock/unlock-last-point . "lock-LP")
    (sniem-goto-last-point . "goto-LP")
    (sniem-move-last-point . "move-LP-p")
    (sniem-goto-next . "lines ↓")
    (sniem-goto-prev . "lines ↑")
    (sniem-find-forward . "find →")
    (sniem-find-backward . "find ←")
    (sniem-up/down-case . "↑/↓ case")
    (save-buffers-kill-terminal . "exit")
    (sniem-yank-in-region . "Y [mark]")
    (sniem-delete-in-region . "D [mark]")
    (sniem-change-in-region . "C [mark]")
    (sniem-paste-in-region . "P [mark]")
    (keyboard-quit . "quit")
    (sniem-append-line . "A-line")
    (sniem-insert-line . "I-line")
    (sniem-delete-char . "D-char")
    (sniem-scroll-up-command . "page ↓")
    (sniem-scroll-down-command . "page ↑")
    (sniem-object-catch-round . "OC-(")
    (sniem-object-catch-square . "OC-[")
    (sniem-object-catch-curly . "OC-{")
    (sniem-replace-char . "R-char")
    (sniem-replace-word . "R-word")
    (eval-last-sexp . "eval")
    (save-buffer . "save")
    (execute-extended-command . "M-x")
    (sniem-open-line . "new-line↓")
    (sniem-open-line-previous . "new-line↑")
    (sniem-cheatsheet . "cheatpage"))
  "The short commands."
  :type 'list
  :group 'sniem)

(defun sniem-cheatsheet ()
  "Switch to the cheatsheet buffer and insert the cheatsheet."
  (interactive)
  (if (null sniem-keyboard-layout)
      (user-error "[Sniem]: You should set `sniem-keyboard-layout' first!")
    (let ((cheatsheet-layout (pcase sniem-keyboard-layout
                               ('dvp sniem-cheatsheet-layout-dvp)
                               ('dvorak sniem-cheatsheet-layout-dvorak)
                               ('colemak sniem-cheatsheet-layout-colemak)
                               ('qwerty sniem-cheatsheet-layout-qwerty))))
      (-> (propertize cheatsheet-layout 'face 'font-lock-comment-face)
          (sniem-cheatsheet--replace)
          (sniem-show-cheatsheet)))))

(defun sniem-show-cheatsheet (cs)
  "Show the cheatsheet."
  (with-current-buffer (get-buffer-create "*Sniem Cheatsheet*")
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (insert cs)
    (insert "\nNOTE:\n"
            (propertize "[mark]" 'face 'font-lock-constant-face)
            " means the operation will edit the regions' content. You can mark a content, "
            "then use operations with [mark] sign to edit them.\n"
            (propertize "LP" 'face 'font-lock-constant-face)
            " means the last-point, all of the sniem operations will use it.")
    (text-mode)
    (toggle-truncate-lines t)
    (display-line-numbers-mode -1)
    (line-number-mode -1)
    (setq-local buffer-read-only t))
  (switch-to-buffer "*Sniem Cheatsheet*")
  (ignore (beginning-of-buffer)))

(defun sniem-cheatsheet--replace (cs)
  "Replace the keys in the cheatsheet."
  (-reduce-from
   (lambda (cs key)
     (let* ((ckey (kbd key))
            (cmd (key-binding ckey))
            tmp)
       (if (and cmd (or (keymapp cmd) (symbolp cmd)))
           (let ((case-fold-search nil))
             (s-replace
              (format (if (setq tmp (alist-get ckey sniem-cheatsheet-special-key-space nil nil 'equal))
                          (concat tmp "[%s]")
                        "      [%s]")
                      key)
              (if (and (keymapp cmd) (equal cmd sniem-leader-keymap))
                  "   leader"
                (sniem-cheatsheet--command cmd ckey))
              cs))
         cs)))
   cs sniem-cheatsheet-keys))

(defun sniem-cheatsheet--command (cmd key)
  "Replace the command to short."
  (let ((scmd (or (alist-get cmd sniem-cheatsheet-short-commands)
                  (s-replace "sniem-" "" (symbol-name cmd))))
        (sp-cmd-space (when (sniem-cheatsheet--special-key-p cmd key)
                        (alist-get key sniem-cheatsheet-special-key-width nil nil 'equal))))
    (when scmd
      (when (string= "undefined" scmd)
        (setq scmd ""))
      (if (<= (length scmd) (or sp-cmd-space 9))
          (-> (->> (or sp-cmd-space 9)
                   number-to-string
                   (concat "% "))
              (concat "s")
              (format scmd))
        (s-truncate (or sp-cmd-space 9) scmd)))))

(defun sniem-cheatsheet--special-key-p (cmd key)
  "Check if the key for CMD is included in `sniem-cheatsheet-special-key-space'."
  (catch 'p
    (mapc `(lambda (k)
             (when (and (equal ,key k) (eq (key-binding k) ',cmd))
               (throw 'p k)))
          '(" " "" ""))))

(provide 'sniem-cheatsheet)

;;; sniem-cheatsheet.el ends here
