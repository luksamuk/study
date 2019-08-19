#!/usr/bin/apl --id 1010

⍝⍝⍝ Getting started

⍝⍝ This is a comment.
⍝⍝ Check the GNU APL keyboard for shortcut hints
⍝⍝ at any time.

⍝ Simple Arithmetic

5+12

18÷3
108÷11

4×7
3.893×7.6

100-95
8-16

3+2 4 11 7 5

1+2 3 4
1+234

6 3 8 1+3
2.5 33.7 12 8÷15
9.8 11.2 17 1.2×1.175

12 3 29 4×1 3 5 2

3×3-1

2 3 1+8÷2 2 2

(2 3 1+8)÷2 2 2

1985 - 1066       ⍝ Difference of two numbers
3 ¯1 ¯7 + ¯4 ¯1 2 ⍝ Sum between two lists with negative numbers

2-3+5             ⍝ This does 3+5, then does 2-8
2 ¯3+5            ⍝ This adds 5 to the number list 2 ¯3

5+4
1 3 4+3 1 6

+12

- 3 ¯6 ¯8 4 12 ¯9

÷1 2 4 10 100

×8 0 ¯3 ¯7 0 4

⌈120.11 12.32 65.01 13.52 - 0.5
⌊99.99 12.82 15.39 48.90 + 0.5

2 ⌈ 6
2 ⌊ 6

6 8 1 ⌈ 3 5 9
6 8 1 ⌊ 3 5 9

⍝ Exercises

4 + 8 × 3 6 2

1.15 × 14 5 78 145

(13 - 8) + 4 6 12 7
⍝ Or...
4 6 12 7 + 13 - 8

((6 × 3) × (4 × 8)) - 5
⍝ Or...
¯5+(6×3)×4×8

- 3 ¯4 ¯12 6

2 7 0 55 ⌈ 33 1 10 13

200 180 230×1.96

⌊((79 84 83 78 74 69 70-32)×5÷9)+0.5
⍝ Or...
⌈¯0.5+(5÷9)×79 84 83 78 74 69 70-32

¯1500+1760×0.9144

⍝ Variables

A ← .175

200×A
A×30.50 12.25 60.30 15.00
⌈ A×30.50 12.25 60.30 15.00

C ← .45359237
17 × C        ⍝ Convert 17 lbs into Kg
⌈C×11×14      ⍝ How many Kgs are there in 11 stones,
              ⍝ then round up

JOE ← ⌈C×11×14

AAA ← 4
ab ← 1
C9999 ← 0
Jack_Smith ← 100

PRICE ← 12.45 5.60 5.99 7.75
+VAT   ← PRICE × A ⍝ A was assigned earlier

)VARS

)WSID

)WSID NEW

)CLEAR

A ← 'APL WILL PROCESS TEXT'
C ← 'CHARACTERS'

NAME ← 'WHAT''S IN A NAME? '

N ← 'NET PRICE'
QTY ← '230'

(ZAK YAK) ← 5

(YEN MARK BUCK) ← 10 20 30

N 10
NAME C

X ← 18
Y ← 3 1985
X Y

NAME X C

'NET PRICE: ' 10

Z ← X Y

Z ← Z+10

CNAME ← 'BASIL '
SNAME ← 'BRUSH'
NAME  ← CNAME SNAME

⍴NAME
1⍴NAME

NAME ← CNAME,SNAME

⍴NAME

PIERRE ← 1 2 3 4
MIREILLE ← 'FILLE'

PIERRE ← (1 2 3) (4 5 6 7)

FRANCOISE ← 'UNE' 'JEUNE' 'FILLE'

PHONES ← 'BILL' 577332 'FRANK' 886331

)CLEAR

(D M Y) ← 22 2 2007

DATE ← 'TODAY''S DATE: '

DATE D M Y

⍝ 1 stone = 14 lbs.
⍝ 1 lb    = 0.454 Kg.
⍝ Let's pretend I weight 11.5 stones.
CONV     ← .454
MYWEIGHT ← ⌊11.5×CONV×14×.9
MYWEIGHT

ITEMS_A ← 2×8 6 12 4
ITEMS_B ← 1.75×16 13 7
ITEMS   ← ⌈1.17×ITEMS_A,ITEMS_B
'PRICE+VAT: ' ITEMS

TEST1 ← 65 72 54 80 67 60 59
TEST2 ← 75 70 60 74 58 61 50
TEST1 ⌈ TEST2

)CLEAR

⍝ Tables

? 100

50 ? 100

⍳100

4 3 ⍴ 10 20 30 40 50 60 70 80 90 100 110 120

DATA ← 12 ? 100
4 3 ⍴ DATA

4 3 ⍴ 1 2 3 4 5

3 5 ⍴ 1

3 3 ⍴ 1 0 0 0

4 4 ⍴ 1,(4 ⍴ 0)

SALES ← 3 3⍴20 13 8 30 43 48 3 50 21
SALES

SALES×10

PRICES ← 2 3 ⍴ 21 2 12 47 33 1

SALES ← 3 2⍴SALES

SALES ← 2 3⍴SALES

TOTAL ← SALES×PRICES
SALES-PRICES

TOTAL ← ⌈¯.5+(5 5⍴25+⍳25)×10×÷5 5⍴⍳25

SALES,PRICES

LITTLE ← 2 2⍴1
MEDIUM ← 2 6⍴5
BIG    ← LITTLE,MEDIUM

ZEROES ← 2 4⍴0
LITTLE ← LITTLE,ZEROES
LITTLE+MEDIUM

LITTLE ← 2 2⍴1
LITTLE ← ZEROES,LITTLE
LITTLE+MEDIUM

+TABLE ← 4 3⍴2 12 15 4 11 7 1 16 8 20 19 9

TABLE[4;3]

TABLE[3;2] ← TABLE[1;2] + TABLE[2;2]

TABLE[1;1 2]
TABLE[1 2;2]

TABLE[1;]
TABLE[;1]

TABLE[;3] ← TABLE[;1] + TABLE[;2]

LIST ← 8 1 90 4
LIST[2]

SALES ← 6 4⍴24?50

+SALES ← 3 6 4⍴72?100
SALES[2;5;4]           ⍝ Plane 2, Row 5, Column 4
SALES[2;;]             ⍝ Plane 2

⍴SALES

TABLE ← 5 3⍴15?20
LIST ← ⍳6
NUM ← 234

⍴TABLE
⍴LIST
⍴NUM

⍴12 61 502 1 26 0 11
⍴'SHAMBOLIOSIS'

⍝ Compare these two.
ALF ← 3 5⍴'ABCDE'
NUM ← 3 5⍴12345

MYNAME ← 'GORSUCH'
⍴MYNAME

3 7⍴MYNAME
3 14⍴MYNAME
3 18⍴MYNAME

MYNAME ← 'GORSUCH '
⍴MYNAME

3 40⍴MYNAME

4 11⍴'ADAMS      CHATER     PRENDERGASTLEE        '

MIXTURE ← 3 3⍴'A' 1 'B' 'C' 2 'D' 'E' 3 'F'

MIXTURE[;2] ← 10×MIXTURE[;2]

NEST ← 2 3⍴(2 2⍴⍳4) (⍳5) 'A NAME' (2 4⍴⍳8) 23 (3 4⍴'NAME')
⍴NEST

≡45          ⍝ Values have depth 0
≡1 2 3       ⍝ Lists have depth 1
≡2 2⍴3 4 5 6 ⍝ Tables too

≡NEST

BIG_NEST ← NEST NEST
⍴BIG_NEST
≡BIG_NEST

⍝ Playing with sizes of character lists
(⍴'ABC','DEF')+⍴'GHI'

⍝ Selecting the first nine numbers in row 1 of a big table
TABLE ← 10 10⍴100?100
TABLE[1;⍳9]

)CLEAR

MILES ← 4 1⍴300 42 25 140

RATES ← 4 1⍴27.5 15 27.5 27.5

+EXPENSES ← .01×RATES×MILES

MILES[3;1] ← 250
+EXPENSES   ← (.01×RATES×MILES)[;1]

⍝ Defining the tables
X ← 3 10⍴30?30
Y ← 3 4⍴30+12?12

⍝ To sum Y into X, we catenate zeroes to Y,
⍝ extending it.
X+Y,3 ((⍴X)[2]-(⍴Y)[2])⍴0

X[3;] ← X[1;]+X[2;]

9 1⍴'APL ROCKS'

)CLEAR

⍝ Writing a function

+/ 1 6 3 4
×/ 1 2 3 4

TABLE ← 3 3⍴⍳9
TABLE
+/ TABLE

+/+/ TABLE

⌈/ 75 72 78 90 69 77 81 88

⌊/ 75 72 78 90 69 77 81 88

X ← ⍳5
(+/ X)÷⍴X

+/TABLE
+/[]TABLE

+/[1]TABLE

∇TRY1
  'Type some numbers: '
  NUM ← ⎕   ⍝ Asks for user input
  'Total is: ' (+/ NUM)
∇

∇TRY2
  'Type some numbers: '
  NUM ← ⎕
  'You have entered' (⍴NUM) 'numbers'
∇

∇AVERAGE
  'Type some numbers:'
  NUM ← ⎕
  'Integer average of these numbers is:' (⌊(+/ NUM)÷⍴NUM)
∇

∇TRY3
  'Type some numbers:'
  NUM ← ⎕
  'You have entered' (⍴NUM) 'numbers'
  'The biggest was' (⌈/ NUM)
  'The smallest was' (⌊/ NUM)
  'Sum of numbers is' (+/ NUM)
  'Integer average of numbers is' (⌊(+/ NUM)÷⍴NUM)
∇

)FNS

)ERASE TABLE X

)WSID ./MyFirstWS.xml

)SAVE

)CLEAR

)LOAD ./MyFirstWS.xml

∇AV X
  (+/ X)÷⍴X
∇

AV 12 7 3 1
AV 3 8 1 4
AV 192 4534 12 0 2

NUM ← ⍳5
AV NUM

∇A SUM B
  A+B
∇

∇R←AV X
  R←(+/ X)÷⍴X
∇

¯3 + AV 3 8 1 4

∇R←A SUM B
  R←A+B
∇

)ERASE NUM
)SAVE
)CLEAR

⎕WA

⍝⍝⍝ APL Concepts

⍝ Data

234.98×3409÷12.4

VAR ← 183.6

⍝ Scalars (no dimensions)
294
'A'

⍝ Vectors (one dimension -- length)
23 8 0 12 3
'ABC'
28 3 'A' 'BC'

⍝ 2D Matrices (two dimensions -- height and length)
⍝ There is no way to write a matrix literal.
4 4⍴7 45 2 89 16 15 10 21 8 0 13 99 83 19 4 27
4 2⍴'WILSO' 393 'ADAMS' 7183 'CAIRN' 87 'SAMSO' 8467

⍝ 3D Matrices (three dimensions)
3 3 4⍴36?100

X1 ← 23 9 144 12 5 0
X2 ← 1 2 'A' 'B' 3 4
2 3⍴23 9 144 12 5 0

NUMS ← 36?100
3 3 4⍴NUMS

6⍴9

⍝ Nested arrays
VAR ← (2 3⍴9) (1 2 3) 'A' 'ABCD' 88 16.1

X ← 1⍴22
Y ← 22

⍴X    ⍝ 1, because X is a vector
⍴Y    ⍝ Empty response, because Y is a scalar

Z ← 1 5⍴12 5 38 3 6   ⍝ When displayed, Z looks like a vector,
⍴Z                    ⍝ but is in fact a 1×5 matrix

)CLEAR

X ← ⍳0   ⍝ X is a vector of zero elements
X        ⍝ Printing X gives an empty response
⍴X       ⍝ Asking for the shape of X gives a zero

⍴45

TAB ← 3 0⍴⍳0
TAB
⍴TAB

X ← 3 4⍴⍳12
+/ X

+/[1] X

)CLEAR

⍝ Indexing in one dimension
X ← 1 45 6 3 9 33 6 0 1 22
X[4] + X[10]

⍝ Indexing in two dimensions
TABLE ← 3 3⍴9?100
TABLE[3;2]         ⍝ Indexing for more than one dimension

⍝ Indexing in three dimensions
DATA ← 4 4 4⍴64?100
DATA[2;1;4]

⍝ Selecting an entire row in tree ways
TABLE[1;1 2 3]
TABLE[1;⍳3]
TABLE[1;]

⍝ Selecting an entire column
TABLE[;2]

⍝ Selecting from anonymous data
(3 8 4)[1+2]

⍝ Selecting from an anonymous string, based on a variable
P ← 2
'ABCDE'[P]

'ABCDE'[4 5 1 4]

'ABCDE'[2 2⍴4 5 1 4]

2⌷'ABCD'

)CLEAR

⍝ Built-in Functions

⌈12.625         ⍝ Ceiling
2⌈8             ⍝ Select greatest number

÷1 2 3 4 5      ⍝ Reciprocal
100÷1 2 3 4 5   ⍝ Divide 100 by each

MAT ← 2 2⍴⍳4
⌹MAT
5 6⌹MAT

TABLE ← 3 3⍴25-9?50
TABLE < 0

≡2 2⍴1 (2 3) (4 5 6 7) (8 (9 10) 11)

't' 'e' 's' 't'≡'test'

≢2 2⍴1 (2 3) (4 5 6 7) (8 (9 10) 11)

('t' 'e') ('s' 't')≢'test'

2∊1 2 3

∊3 3 3⍴⍳27

'ana' ⍷ 'banana'

⍳9
3 3⍴⍳9

X ← 0 0 5 3
X[(0≠0 0 5 3)⍳1] ⍝ Get first non-null element of X

~1 0 1
1 0 1∨0 0 1
1 0 1∧0 0 1
1 0 1⍱0 0 1
1 0 1⍲0 0 1

(5 > 4) ∧ 1 < 3

X ← 3 3 3⍴⍳27 ⍝ A cube
,X

,[1 2]X

(3 3⍴⍳9),(3 3⍴9+⍳9)

⌽0 0 5 7

2⌽3 3⍴⍳9
¯2⌽3 3⍴⍳9

⍉3 3⍴⍳9

2 1 3⍉3 3 3⍴⍳27    ⍝ Swap axes 1 and 2

↑3 1 2

2↑⌽⍳4
¯7↑⌽⍳4

⊂2 2⍴⍳4
⍴⊂2 2⍴⍳4

0 1 1 0⊂⍳4

⊃(⍳4) 2 3

X ← 4⍴⊂(4 4⍴16?100)   ⍝ List of four enclosed 4x4 matrices
2 (2 2)⊃X             ⍝ Pick 2nd matrix, then pick element [2;2]

2 3⊢4 5
⊢/ 6 7 8 9

2 3⊣4 5
⊣/ 6 7 8 9

LIST ← 10?100
LIST[⍋LIST]

TEXT ← 'Banana'
TEXT['an'⍋TEXT]

LIST ← 10?100
TEXT ← 'Banana'

LIST[⍒LIST]
TEXT['an'⍒TEXT]

2⊥0 0 1 0 1
16⊥2 1
24 60 60⊥2 46 40  ⍝ Time conversion! 2h46m40s into total seconds

2 2 2 2⊤5 7 12
24 60 60⊤10000   ⍝ Mixed radix; convert 10000 seconds to h m s

LIST ← 25-(5?50) ◊ (÷LIST)

⍎'X ← 10×3 3⍴⍳9 ◊ ÷X'

⍕1 2 3

6 2⍕3.25 3.002
⍝ 8 2⍕1234   ⍝ Not wide enough

TABLE ← 3 4⍴⍳12

2 3⌷TABLE
2⌷[1] TABLE
2⌷[2] TABLE

⍝ These are a match, since they are numeric vectors.
⍬≡⍳0
⍬≡0⍴0

⍝ These do not match.
⍬≡0 0⍴0  ⍝ Not a vector
⍬≡''     ⍝ Not numeric

⍝ Built-in Operators

⍝ These two operations are equivalent
22 + 93 + 4.6 + 10 + 3.3
+/22 93 4.6 10 3.3        ⍝ Reduce using plus

+\22 93 4.6 10 3.3        ⍝ Scan using plus
22 (22+93) (115+4.6) (119.6+10) (129.6+3.3) ⍝ Equivalent calculation

TABLE ← 3 5⍴15?30
+⌿ TABLE

+⍀ TABLE

1 0 1 1 0 1 / 'ABCDEF'

TABLE ← 2 3⍴⍳6
⍝ Insert new columns (axis 2).
⍝ New columns indicated by zeroes.
1 0 1 0 1\[2]TABLE

TABLE ← 3 4⍴⍳12
1 0 2⌿TABLE      ⍝ Remove 2nd row, duplicate 3rd row

TABLE ← 3 4⍴⍳12
1 0 1 0 1 0 0⍀TABLE

1 2 3∘.+4 5 6

(⍳4)∘.*⍳4

X←3 3⍴9?100
Y←3 3⍴9?100

⍝ 1. Each row of X is multiplied by each column of Y;
⍝ 2. The result is reduced through a sum.
X+.×Y

⍴¨(⍳3)(⍳2)(⍳5)   ⍝ Find the length of each vector

TABLE ← 2 3⍴⍳6  ⍝ A matrix of 2×3 (two dimensions)

⍝ Reduce with + on the second dimension. This gives a
⍝ list of two numbers, each being the sum of numbers
⍝ along the COLUMNS (dimension 2, last one) of each
⍝ row of the matrix.
+/TABLE

⍝ This reduction specifies that the sum should occur
⍝ along the ROWS (dimension 1) of a column of the
⍝ matrix, therefore it gives a list of three numbers.
+/[1]TABLE

⍝ User-defined functions

∇SD X
  SUM ← +/X
  AVG ← SUM÷⍴X
  DIFF ← AVG-X
  SQDIFF ← DIFF⋆2
  SQAVG ← (+/SQDIFF)÷⍴SQDIFF
  RESULT ← SQAVG⋆0.5
∇

SD 12 45 20 68 92 108

∇R ← SD X
  SUM ← +/X
  AVG ← SUM÷⍴X
  DIFF ← AVG-X
  SQDIFF ← DIFF⋆2
  SQAVG ← (+/SQDIFF)÷⍴SQDIFF
  R ← SQAVG⋆0.5
∇

∇R ← X (LOP OPERATE ROP) Y

)CLEAR   ⍝ Clear the workspace

∇R ← SD X;SUM;AVG;DIFF;SQDIFF;SQAVG
  SUM ← +/X
  AVG ← SUM÷⍴X
  DIFF ← AVG-X
  SQDIFF ← DIFF⋆2
  SQAVG ← (+/SQDIFF)÷⍴SQDIFF
  R ← SQAVG⋆0.5
∇

∇R ← SD X;SQDIFF
  SQDIFF ← (X-(+/X)÷⍴X)⋆2
  R ← ((+/SQDIFF)÷⍴SQDIFF)⋆0.5
∇
