class LandmarkCounter:
  _landmarkDict = {}
  _landmarkList = []
  def __init__(self,dict,list):
    self._landmarkDict = dict
    self._landmarkList = list
 
  def getProb(self,landmark,i,j,outcome):
    posDict = self._landmarkDict[landmark]
    if (i,j) not in posDict.keys():
      return 0.0
    
    num = float(posDict[(i,j)][outcome] + 1)
    tot = sum([x+1 for x in posDict[(i,j)].values()])
    return num/tot
    
  def getLandmarks(self):
    return self._landmarkList  # in correct order
 
  def incrementCounts(self,i,j,yesList):
    for key in self._landmarkDict.keys():
      posDict = self._landmarkDict[key]
      if (i,j) not in posDict:
        posDict[(i,j)] = {'yes':0,'no':0}
      if key in yesList:
        posDict[(i,j)]['yes'] = posDict[(i,j)]['yes'] + 1
      else:
        posDict[(i,j)]['no'] = posDict[(i,j)]['no'] + 1
        
  def printCounts(self):
    for key in self._landmarkDict.keys():
      print key + ":"
      posdict = self._landmarkDict[key]
      for pos in posdict.keys():
        print "  " , pos , ":" , posdict[pos] 