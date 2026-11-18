import json,os,sys
from urllib.request import Request,urlopen
P=' PNBRQK  pnbrqk';V=(0,100,320,330,500,900,10000)
S=[i for i in range(128)if not i&136]
def ray(i,d):
 while not(i:=i+d)&136:yield i
R={i:[tuple(ray(i,d))for d in(16,-16,1,-1,17,15,-15,-17)]for i in S}
J={i:[(i+d,)for d in(33,31,18,14,-14,-18,-31,-33)if not(i+d)&136]for i in S}
T={}
for s in(0,8):
 for i in S:
  x=i&7;y=i//16^(7 if s else 0);a=7-abs(2*x-7);z=a+7-abs(2*y-7)
  T[s,i]=(0,100+8*y+2*a,296+6*z,330+3*z,500+16*(y==6),900+z,-4*z+24*(y==0 and x in(2,6)))

def sq(u):return ord(u[0])-97+16*(int(u[1])-1)
A={i:chr(97+(i&7))+str(i//16+1)for i in S}
def uci(m):i,j,t=m;return A[i]+A[j]+(P[t].lower()if t else'')
def fen(f):
 a,s,c,e,h,_=f.split();b=bytearray(128)
 i=112
 for row in a.split('/'):
  for x in row:
   if x.isdigit():i+=int(x)
   else:b[i]=P.index(x);i+=1
  i-=24
 return b,8*(s=='b'),sum(1<<n for n,x in enumerate('KQkq')if x in c),sq(e)if e!='-'else-1,int(h)
def hit(b,k,s):
 p=k-16+4*s
 for j in(p-1,p+1):
  if not j&136 and b[j]==s+1:return 1
 for j, in J[k]:
  if b[j]==s+2:return 1
 for n,r in enumerate(R[k]):
  for j in r:
   if x:=b[j]:
    x^=s
    if x==5 or x==(4 if n<4 else 3)or x==6 and j==r[0]:return 1
    break
C={0:2,4:3,7:1,112:8,116:12,119:4}
def play(g,m):
 b,s,c,e,h=g
 i,j,t=m;n=b.copy();p=b[i]&7;n[i]=0;n[j]=s|(t or p);d=16-4*s
 if p==1 and j==e:n[j-d]=0
 if p==6 and abs(i-j)==2:
  a,k=(i+3,i+1)if j>i else(i-4,i-1);n[k]=n[a];n[a]=0
 return n,s^8,c&~(C.get(i,0)|C.get(j,0)),i+d if p==1 and abs(j-i)==32 else-1,0 if p==1 or b[j]else h+1
def gen(b,s,c,e,q,k):
 d=16-4*s;o=s^8
 for i in S:
  if not(x:=b[i])or x&8!=s:continue
  p=x&7
  if p==1:
   j=i+d
   if not q and i//16==(6 if s else 1)and not(b[j]or b[j+d]):yield i,j+d,0
   for t in(j-1,j,j+1):
    if not t&136 and (not q and not b[t]if t==j else t==e or b[t]and b[t]&8==o and b[t]&7!=6):
     for v in((5,4,3,2)if t//16 in(0,7)else(0,)):yield i,t,v
  else:
   for r in(J[i]if p==2 else R[i][:4]if p==4 else R[i][4:]if p==3 else R[i]):
    for j in r:
     x=b[j]
     if (x&8==o and x&7!=6)if x else not q:yield i,j,0
     if x or p==6:break
 c=c>>s//4&3
 if not q and c and not hit(b,k,o):
  for d,r,f in((1,3,1),(-1,-4,2)):
   n=b.copy();n[k]=0;n[k+d]=s+6
   if c&f and b[k+r]==s+4 and not any(b[k+j*d]for j in range(1,abs(r)))and not hit(n,k+d,o):yield k,k+2*d,0
def legal(g,q=0):
 b,s,c,e,_=g;k=b.index(s+6);ms=gen(b,s,c,e,q,k)
 def rank(m):
  i,j,t=m;p=b[i]&7;v=b[j]&7 or(j==e and p==1)
  return V[v],-V[p]if v else 0,V[t]+T[s,j][p]-T[s,i][p]
 ms=sorted(ms,key=rank,reverse=True)
 for m in ms:
  n=play(g,m)
  if not hit(n[0],m[1] if m[0]==k else k,s^8):yield m,n
def key(g):
 b,s,c,e,_=g
 if e>=0:
  k=b.index(s+6);p=e-16+4*s
  if not any(not i&136 and b[i]==s+1 and not hit(play(g,(i,e,0))[0],k,s^8)for i in(p-1,p+1)):e=-1
 return bytes(b),s,c,e
def val(b,s):return sum(T[x&8,i][x&7]*(1 if x&8==s else-1)for i in S if(x:=b[i]))
def search(g,d,a,b,h,p):
 x,s,c,e,f=g;z=hit(x,x.index(s+6),s^8);k=key(g);n=h.get(k,0)
 if d<=0 and not z:
  if f>=100 or n>=2:return 0
  if not any(legal(g)):return 0
  a=max(a,val(x,s))
  if a>=b or p>36:return a
  it=legal(g,1)
 else:it=legal(g)
 m=next(it,None)
 if not m:return p-30000 if z else a if d<=0 else 0
 if f>=100 or n>=2:return 0

 h[k]=n+1
 while m:
  a=max(a,-search(m[1],d-1,-b,-a,h,p+1))
  if a>=b:break
  m=next(it,None)
 h[k]=n
 return a
def choose(f,allowed=()):
 g=fen(f);h={}
 a=-32000;best=None
 for m,n in legal(g):
  if allowed and uci(m)not in allowed:continue
  v=-search(n,2,-32000,-a,h,1)
  if v>a:a,best=v,m
 if best:return uci(best)
def post(path,**data):
 with urlopen(Request(os.environ['MAKEFUN_ENV_URL']+path,json.dumps(data).encode(),{'content-type':'application/json'}))as x:return json.load(x)
if __name__=='__main__':
 while(g:=post('/game'))['status']!='done':
  while g['status']=='your_move':
   g=post('/move',game=g['game'],move=choose(g['fen'],g['legal_moves']))
  print('game {game}: {outcome} by {reason}'.format(**g),file=sys.stderr)
 print('done: {wins} won, {draws} drawn, {losses} lost'.format(**g),file=sys.stderr)
