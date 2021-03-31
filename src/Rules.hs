module Rules where

type Cell = Char
type Rule = Cell -> Cell -> Cell -> Char

rule30 :: Rule
rule30 '*' ' ' ' ' = '*'
rule30 ' ' '*' ' ' = '*'
rule30 ' ' ' ' '*' = '*'
rule30 ' ' '*' '*' = '*'
rule30 '*' '*' '*' = ' '
rule30 '*' '*' ' ' = ' '
rule30 ' ' ' ' ' ' = ' '
rule30 '*' ' ' '*' = ' '

rule90 :: Rule
rule90 ' ' ' ' '*' = '*'
rule90 ' ' '*' ' ' = ' '
rule90 '*' ' ' ' ' = '*'
rule90 '*' '*' ' ' = '*'
rule90 '*' '*' '*' = ' '
rule90 ' ' '*' '*' = '*'
rule90 ' ' ' ' ' ' = ' '
rule90 '*' ' ' '*' = ' '

rule110 :: Rule
rule110 '*' '*' '*' = ' '
rule110 '*' '*' ' ' = '*'
rule110 '*' ' ' ' ' = ' '
rule110 ' ' '*' '*' = '*'
rule110 ' ' '*' ' ' = '*'
rule110 ' ' ' ' '*' = '*'
rule110 ' ' ' ' ' ' = ' '
rule110 '*' ' ' '*' = '*'