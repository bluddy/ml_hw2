
      
class Clique:

  def __init__(self,id):
    self._id = id
    self._nodes = id.split(",")
    self._edges = []

  def addEdge(self,id):
    self._edges.append(id)
  
  def printInfo(self):
    print self._id
    for id in self._edges:
      print "--" + id
      
cliqueDict = {}
f = open('cliques.txt','r')

numCliques = int(f.readline())
print numCliques

for i in xrange(numCliques):
  s = f.readline().strip('\n')
  c = Clique(s)
  cliqueDict[s] = c

for line in f:
  s = line.split("--")
  z = [x.strip() for x in s]
  c1 = cliqueDict[z[0]]
  c2 = cliqueDict[z[1]]
  print c1._id, c2._id
  c1.addEdge(z[1])
  c2.addEdge(z[0])
  
f.close()
for c in cliqueDict.values():
  c.printInfo()



