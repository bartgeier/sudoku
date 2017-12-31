cd C:\Projekts\Haskell\SudokuPuzzle\
rem ghc -o Sudoku main.hs
rem ghc -o Sudoku_test Sudoku_test.hs
rem -Wall -Werror
set testKit=TestKit/TestKit.hs 
set tool=Tool/Tool_List.hs
set testTool=unitTest/Tool/Test_Tool_List.hs
set testField=unitTest/Board/Test_Board.hs unitTest/Board/Test_Cell.hs unitTest/Board/Test_Field.hs unitTest/Board/Test_Block.hs 
set testRule=unitTest/Test_Rule.hs
set testAlgo=unitTest/Algorithm/test_BruteforceBacktrack.hs  unitTest/Algorithm/Test_SortedBacktrack.hs
set hs=Board/Board.hs Board/Field/Field.hs Board/Field/Internal.hs Board/Cell.hs Board/Block/Block.hs Board/Block/Internal.hs Rule/Rule.hs Rule/Internal.hs
set algo=Algorithm/BruteforceBacktrack/BruteforceBacktrack.hs Algorithm/BruteforceBacktrack/Internal.hs Algorithm/SortedBacktrack/SortedBacktrack.hs
rem ghc -o Sudoku_test Sudoku_test.hs -idirs %hs% %testKit% %testField% %testRule% -Wall -Werror
ghc -o Sudoku_test Sudoku_test.hs -idirs %hs% %algo% %testKit% %tool% %testTool% %testField% %testRule% %testAlgo% -Wall
rem ghc -o Sudoku_test Sudoku_test.hs -idirs %hs% %testKit% %testField% %testRule%
Sudoku_test.exe
pause 