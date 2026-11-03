import asyncio, json, os, sys, subprocess, time, base64, urllib.request
import websockets
O=os.path.dirname(os.path.abspath(__file__))
os.makedirs(O+"/shots",exist_ok=True)
JS=r"""(() => {
const o={iw:innerWidth, docSW:document.documentElement.scrollWidth, bodySW:document.body.scrollWidth, bodyCW:document.body.clientWidth, H:document.documentElement.scrollHeight};
const isSc=el=>{const x=getComputedStyle(el).overflowX;return x==='auto'||x==='scroll'};
const inSc=el=>{let p=el.parentElement;while(p&&p!==document.body){if(isSc(p))return true;p=p.parentElement}return false};
o.tables=[...document.querySelectorAll('table')].map(t=>{const c=t.parentElement;const tr=t.getBoundingClientRect(),cr=c.getBoundingClientRect();return {tableW:+tr.width.toFixed(1),contW:c.clientWidth,contScrollW:c.scrollWidth,needsScroll:c.scrollWidth>c.clientWidth+0.5,tableRight:+tr.right.toFixed(1),contRight:+cr.right.toFixed(1),tableLeft:+tr.left.toFixed(1),contLeft:+cr.left.toFixed(1),outside:(tr.right>cr.right+1&&!isSc(c))||tr.left<cr.left-1}});
o.spills=[];
for(const el of document.body.querySelectorAll('*')){if(inSc(el))continue;const r=el.getBoundingClientRect();if(!r.width&&!r.height)continue;if(r.right>innerWidth+1||r.left<-1){o.spills.push({tag:el.tagName,t:(el.textContent||'').trim().slice(0,40),l:+r.left.toFixed(1),r:+r.right.toFixed(1)});if(o.spills.length>8)break}}
const chrect=(n,i)=>{const g=document.createRange();g.setStart(n,i);g.setEnd(n,i+1);const rs=[...g.getClientRects()].filter(x=>x.width>0);return rs[0]||null};
o.pairs=[];o.splits=[];
const UN=/(\d)([  ])(kilos?|cursos?)/g, EL=/\b(El|La)([  ])(Bosque|Pradera)/g, MU=/^([  ]*)(kilos?|cursos?)/;
const w=document.createTreeWalker(document.body,NodeFilter.SHOW_TEXT);let n;
while(n=w.nextNode()){const s=n.nodeValue;const intd=!!n.parentElement.closest('td,th');
 let m;UN.lastIndex=0;
 while(m=UN.exec(s)){const a=chrect(n,m.index),b=chrect(n,m.index+2);const nb=m[2]===' ';
  if(a&&b){const sp=Math.abs((a.top+a.bottom)/2-(b.top+b.bottom)/2)>a.height*0.5;o.pairs.push({k:'cifra-unidad',nb,intd,sp,t:m[0]});if(sp)o.splits.push({k:'cifra-unidad',nb,intd,t:s.slice(Math.max(0,m.index-25),m.index+14)})}}
 EL.lastIndex=0;
 while(m=EL.exec(s)){const a=chrect(n,m.index+1),b=chrect(n,m.index+3);const nb=m[2]===' ';
  if(a&&b){const sp=Math.abs((a.top+a.bottom)/2-(b.top+b.bottom)/2)>a.height*0.5;o.pairs.push({k:'El-Bosque',nb,intd,sp,t:m[0]});if(sp)o.splits.push({k:'El-Bosque',nb,intd,t:s.slice(Math.max(0,m.index-25),m.index+20)})}}
 const mm=MU.exec(s);const ps=n.previousSibling;
 if(mm&&ps&&ps.nodeType===1&&ps.tagName.toUpperCase()==='MATH'){const b=chrect(n,mm[1].length);const mr=ps.getBoundingClientRect();
  if(b){const cy=(b.top+b.bottom)/2;const sp=cy<mr.top-1||cy>mr.bottom+1;const nb=mm[1].indexOf(' ')>=0;o.pairs.push({k:'formula-unidad',nb,intd,sp,t:(ps.textContent||'').slice(-20)+'|'+mm[2]});if(sp)o.splits.push({k:'formula-unidad',nb,intd,t:'MATH '+(ps.querySelector('annotation')||ps).textContent.slice(-25)+' '+mm[2]})}}
}
o.mathCount=document.querySelectorAll('math').length;
o.nbsp=(document.body.innerText.match(/ /g)||[]).length;
return JSON.stringify(o)})()"""
class CDP:
    def __init__(s,ws): s.ws,s.n,s.pend,s.ev=ws,0,{},asyncio.Queue()
    async def start(s): s.t=asyncio.create_task(s._rd())
    async def _rd(s):
        try:
            async for m in s.ws:
                m=json.loads(m)
                if "id" in m and m["id"] in s.pend: s.pend.pop(m["id"]).set_result(m)
                else: await s.ev.put(m)
        except Exception: pass
    async def send(s,method,**p):
        s.n+=1;i=s.n;f=asyncio.get_running_loop().create_future();s.pend[i]=f
        await s.ws.send(json.dumps({"id":i,"method":method,"params":p}))
        r=await asyncio.wait_for(f,60)
        if "error" in r: raise RuntimeError(r["error"])
        return r.get("result",{})
    async def wev(s,name,to):
        end=time.time()+to
        while True:
            l=end-time.time()
            if l<=0:return False
            try:m=await asyncio.wait_for(s.ev.get(),l)
            except asyncio.TimeoutError:return False
            if m.get("method")==name:return True
async def job(port,sem,v,seed,vw):
    f=f"{O}/html/{v}/s{seed:03d}/ej1.html";tid=None
    async with sem:
        try:
            async def run():
                nonlocal tid
                t=json.loads(urllib.request.urlopen(urllib.request.Request(f"http://127.0.0.1:{port}/json/new?about:blank",method="PUT"),timeout=10).read());tid=t["id"]
                async with websockets.connect(t["webSocketDebuggerUrl"],max_size=None) as ws:
                    c=CDP(ws);await c.start()
                    await c.send("Page.enable");await c.send("Runtime.enable")
                    await c.send("Emulation.setDeviceMetricsOverride",width=vw,height=900,deviceScaleFactor=1,mobile=False)
                    await c.send("Page.navigate",url="file://"+f);await c.wev("Page.loadEventFired",25)
                    await c.send("Runtime.evaluate",expression="document.fonts.ready.then(()=>1)",awaitPromise=True)
                    await asyncio.sleep(0.15)
                    r=await c.send("Runtime.evaluate",expression=JS,returnByValue=True)
                    if "exceptionDetails" in r: raise RuntimeError(json.dumps(r["exceptionDetails"])[:300])
                    m=json.loads(r["result"]["value"])
                    h=min(m["H"],9000)
                    sh=await c.send("Page.captureScreenshot",format="png",captureBeyondViewport=True,clip={"x":0,"y":0,"width":vw,"height":h,"scale":1})
                    open(f"{O}/shots/{v}_s{seed:03d}_{vw}.png","wb").write(base64.b64decode(sh["data"]))
                    return m
            m=await asyncio.wait_for(run(),90)
            return dict(v=v,seed=seed,vw=vw,m=m)
        except Exception as e: return dict(v=v,seed=seed,vw=vw,error=repr(e)[:300])
        finally:
            if tid:
                try:urllib.request.urlopen(f"http://127.0.0.1:{port}/json/close/{tid}",timeout=5).read()
                except Exception:pass
async def main():
    port=9355
    p=subprocess.Popen(["chromium","--headless","--no-sandbox","--disable-gpu","--hide-scrollbars","--force-device-scale-factor=1",f"--remote-debugging-port={port}","--user-data-dir="+O+"/prof","--no-first-run","about:blank"],stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
    for _ in range(150):
        try:urllib.request.urlopen(f"http://127.0.0.1:{port}/json/version",timeout=1).read();break
        except Exception:time.sleep(0.2)
    sem=asyncio.Semaphore(6)
    try:
        return await asyncio.gather(*[job(port,sem,v,s,vw) for s in range(1,101) for v in("ref","nuevo") for vw in(360,320)])
    finally:p.terminate()
res=asyncio.run(main())
json.dump(res,open(O+"/metricas.json","w"))
print(len(res),"errores:",sum('error' in r for r in res),[r for r in res if 'error' in r][:3])
print("iw:",sorted({r['m']['iw'] for r in res if 'm' in r}))
