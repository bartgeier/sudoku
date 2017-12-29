

track(board, (x_,y_), step, dictonary) {
      (x,y) = step(board (x,y));
      if((x,y) == (Nothing,Nothing)) {
            return board;
      }
      if (notFix(atXY(board,(x,y)) {
            newBoard = setCell(board (x,y) dictonary);
            return newBoard;
      }
      newBoard = track(board, (x,y), step, dictonary);
      return newBoard;
}

setCell(board (x,y) dictonary)
      i = nextIndex(dictonary, board, (x,y));
      for (; i < grad^2; i++) {
            if (allowed(board, (x,y), dictonary[i])) {
                  bord = replaceCell(board,(x,y), dictonary[i]);
                  newBoard = track(board, (x,y), next, dictonary);
                  return newBoard;
            }
      }    
      newBoard = track(board, (x,y), back, dictonary);
      return newBoard;
}

setCell(board (x,y) dictonary)
      cell = nextCell(board, (x,y),dictonary);
      while (cell != Empty) {
            if (allowed(board, (x,y), cell)) {
                  bord = replaceXY(board,(x,y), cell);
                  newBoard = track(board, (x,y), next, dictonary);
                  return newBoard;
            }
      }    
      newBoard = track(board, (x,y), back, dictonary);
      return newBoard;
}

next(board, (x,y))
back(board, (x,y)
atXY(board, (x,y)
notFix(cell)
allowed(board, (x,y), cell)
replaceXY