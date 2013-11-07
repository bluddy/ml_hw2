class MotionCounter:
  _numRows = 0
  _numCols = 0
  _motionDict = {'MoveNorth': {'success':0,'fail':0},
                 'MoveSouth': {'success':0,'fail':0},
                 'MoveEast':  {'success':0,'fail':0},
                 'MoveWest':  {'success':0,'fail':0}}
  def __init__(self,rows,cols):
    self._numRows = rows
    self._numCols = cols
    
  def printCounts(self):
    for key in self._motionDict.keys():
      print key, ":"
      for dir in self._motionDict[key].keys():
        print "  ", dir, ":", self._motionDict[key][dir]

    
  def getProb(self,outcome,dir):
    num = float(self._motionDict[dir][outcome] + 1)
    tot = sum([x+1 for x in self._motionDict[dir].values()])
    return num / tot
    
  def incrementCounts(self, i_new,j_new,i_old,j_old, dir):
    if dir == 'MoveNorth':
      # sanity check:
      if j_new != j_old:
        print "Col changed on MoveNorth! bad!!!!"
        raise Exception
      # real work
      counts = self._motionDict['MoveNorth']
      correct = ((i_old % self._numRows) + 1) % self._numRows
      if correct == 0:
        correct = self._numRows
      if i_new == correct:
        counts['success'] = counts['success'] + 1
      elif i_new == i_old:
        counts['fail'] = counts['fail'] + 1
      else:
        # sanity check
        print "Row changed in an unexpected manner! bad!"
        raise Exception
    if dir == 'MoveSouth':
      # sanity check:
      if j_new != j_old:
        print "Col changed on MoveSouth! bad!!!!"
        raise Exception
      # real work
      counts = self._motionDict['MoveSouth']
      # mod subtraction trick
      correct = ((i_old % self._numRows) + (self._numRows - 1)) % self._numRows
      if correct == 0:
        correct = self._numRows
      if i_new == correct:
        counts['success'] = counts['success'] + 1
      elif i_new == i_old:
        counts['fail'] = counts['fail'] + 1
      else:
        # sanity check
        print "Row changed in an unexpected manner! bad!"
        raise Exception
    if dir == 'MoveEast':
      # sanity check:
      if i_new != i_old:
        print "Row changed on MoveEast! bad!!!!"
        raise Exception
      # real work
      counts = self._motionDict['MoveEast']
      correct = ((j_old % self._numCols) + 1) % self._numCols
      if correct == 0:
        correct = self._numCols
      if j_new == correct:
        counts['success'] = counts['success'] + 1
      elif j_new == j_old:
        counts['fail'] = counts['fail'] + 1
      else:
        # sanity check
        print "Col changed in an unexpected manner! bad!"
        raise Exception  
    if dir == 'MoveWest':
      # sanity check:
      if i_new != i_old:
        print "Row changed on MoveWest! bad!!!!"
        raise Exception
      # real work
      counts = self._motionDict['MoveWest']
      correct = ((j_old % self._numCols) + (self._numCols -1)) % self._numCols
      if correct == 0:
        correct = self._numCols
      if j_new == correct:
        counts['success'] = counts['success'] + 1
      elif j_new == j_old:
        counts['fail'] = counts['fail'] + 1
      else:
        # sanity check
        print "Col changed in an unexpected manner! bad!"
        raise Exception 